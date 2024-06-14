use super::{
    state::{ParserState, SASS_CTX_ALLOW_DIV},
    Parser,
};
use crate::{
    ast::*,
    bump,
    config::Syntax,
    eat,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util, Parse,
};

const PRECEDENCE_MULTIPLY: u8 = 6;
const PRECEDENCE_PLUS: u8 = 5;
const PRECEDENCE_RELATIONAL: u8 = 4;
const PRECEDENCE_EQUALITY: u8 = 3;
const PRECEDENCE_AND: u8 = 2;
const PRECEDENCE_OR: u8 = 1;

type SassParams<'s> = (
    Vec<SassParameter<'s>>,
    Option<SassArbitraryParameter<'s>>,
    Vec<Span>, // comma spans
    usize,     // end pos
);

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_maybe_sass_list(
        &mut self,
        allow_comma: bool,
    ) -> PResult<ComponentValue<'s>> {
        use util::ListSeparatorKind;

        let single_value = if allow_comma {
            self.parse_maybe_sass_list(false)?
        } else if let Token::Exclamation(..) = peek!(self).token {
            self.parse().map(ComponentValue::ImportantAnnotation)?
        } else {
            self.parse_sass_bin_expr(/* allow_comparison */ true)?
        };

        let mut elements = vec![];
        let mut comma_spans: Option<Vec<_>> = None;
        let mut separator = ListSeparatorKind::Unknown;
        let mut end = single_value.span().end;
        loop {
            match peek!(self).token {
                Token::LBrace(..)
                | Token::RBrace(..)
                | Token::RParen(..)
                | Token::Semicolon(..)
                | Token::Colon(..)
                | Token::Dedent(..)
                | Token::Linebreak(..)
                | Token::DotDotDot(..)
                | Token::Eof(..) => break,
                Token::Comma(..) => {
                    if !allow_comma {
                        break;
                    }
                    if separator == ListSeparatorKind::Space {
                        break;
                    } else {
                        if separator == ListSeparatorKind::Unknown {
                            separator = ListSeparatorKind::Comma;
                        }
                        let TokenWithSpan { span, .. } = bump!(self);
                        end = span.end;
                        if let Some(spans) = &mut comma_spans {
                            spans.push(span);
                        } else {
                            comma_spans = Some(vec![span]);
                        }
                    }
                }
                Token::Exclamation(..) => {
                    if let Ok(important_annotation) = self.try_parse(ImportantAnnotation::parse) {
                        if end < important_annotation.span.start
                            && matches!(separator, ListSeparatorKind::Unknown)
                        {
                            separator = ListSeparatorKind::Space;
                        }
                        end = important_annotation.span.end;
                        elements.push(ComponentValue::ImportantAnnotation(important_annotation));
                    } else {
                        break;
                    }
                }
                _ => {
                    if separator == ListSeparatorKind::Unknown {
                        separator = ListSeparatorKind::Space;
                    }
                    let item = if separator == ListSeparatorKind::Comma {
                        self.parse_maybe_sass_list(false)?
                    } else {
                        self.parse_sass_bin_expr(/* allow_comparison */ true)?
                    };
                    end = item.span().end;
                    elements.push(item);
                }
            }
        }

        if elements.is_empty() && separator != ListSeparatorKind::Comma {
            // If there is a trailing comma it can be a Sass list,
            // though there is only one element.
            Ok(single_value)
        } else {
            debug_assert_ne!(separator, ListSeparatorKind::Unknown);

            let span = Span {
                start: single_value.span().start,
                end,
            };
            elements.insert(0, single_value);
            Ok(ComponentValue::SassList(SassList {
                elements,
                comma_spans,
                span,
            }))
        }
    }

    pub(super) fn parse_sass_bin_expr(
        &mut self,
        allow_comparison: bool,
    ) -> PResult<ComponentValue<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));
        self.parse_sass_bin_expr_recursively(0, allow_comparison)
    }

    fn parse_sass_bin_expr_recursively(
        &mut self,
        precedence: u8,
        allow_comparison: bool,
    ) -> PResult<ComponentValue<'s>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            self.parse_sass_unary_expression()?
        } else {
            self.parse_sass_bin_expr_recursively(precedence + 1, allow_comparison)?
        };

        // delimiter can't be calculated
        if left.is_delimiter() {
            return Ok(left);
        }

        loop {
            let operator = match peek!(self) {
                TokenWithSpan {
                    token: Token::Asterisk(..),
                    ..
                } if precedence == PRECEDENCE_MULTIPLY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Multiply,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Solidus(..),
                    ..
                } if precedence == PRECEDENCE_MULTIPLY
                    && (self.state.sass_ctx & SASS_CTX_ALLOW_DIV != 0
                        || matches!(
                            left,
                            ComponentValue::SassParenthesizedExpression(..)
                                | ComponentValue::SassBinaryExpression(..)
                                | ComponentValue::SassUnaryExpression(..)
                                | ComponentValue::SassVariable(..)
                                | ComponentValue::Function(..)
                                | ComponentValue::SassQualifiedName(..)
                        )) =>
                {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Division,
                        span: bump!(self).span,
                    }
                }
                TokenWithSpan {
                    token: Token::Percent(..),
                    ..
                } if precedence == PRECEDENCE_MULTIPLY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Modulo,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Plus(..),
                    ..
                } if precedence == PRECEDENCE_PLUS => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Plus,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Minus(..),
                    ..
                } if precedence == PRECEDENCE_PLUS => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Minus,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::GreaterThan(..),
                    ..
                } if allow_comparison && precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::GreaterThan,
                        span: bump!(self).span,
                    }
                }
                TokenWithSpan {
                    token: Token::GreaterThanEqual(..),
                    ..
                } if allow_comparison && precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::GreaterThanOrEqual,
                        span: bump!(self).span,
                    }
                }
                TokenWithSpan {
                    token: Token::LessThan(..),
                    ..
                } if allow_comparison && precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::LessThan,
                        span: bump!(self).span,
                    }
                }
                TokenWithSpan {
                    token: Token::LessThanEqual(..),
                    ..
                } if allow_comparison && precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::LessThanOrEqual,
                        span: bump!(self).span,
                    }
                }
                TokenWithSpan {
                    token: Token::EqualEqual(..),
                    ..
                } if precedence == PRECEDENCE_EQUALITY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::EqualsEquals,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::ExclamationEqual(..),
                    ..
                } if precedence == PRECEDENCE_EQUALITY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::ExclamationEquals,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Ident(token),
                    ..
                } if token.raw == "and" && precedence == PRECEDENCE_AND => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::And,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Ident(token),
                    ..
                } if token.raw == "or" && precedence == PRECEDENCE_OR => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Or,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Number(token),
                    span,
                } if precedence == PRECEDENCE_PLUS
                    && (token.raw.starts_with('+')
                        || token.raw.starts_with('-') && span.start == left.span().end) =>
                {
                    let (number, number_span) = expect!(self, Number);
                    let op = SassBinaryOperator {
                        kind: if number.raw.starts_with('+') {
                            SassBinaryOperatorKind::Plus
                        } else {
                            SassBinaryOperatorKind::Minus
                        },
                        span: Span {
                            start: number_span.start,
                            end: number_span.start + 1,
                        },
                    };
                    let span = Span {
                        start: left.span().start,
                        end: number_span.end,
                    };
                    let right = {
                        let span = Span {
                            start: number_span.start + 1,
                            end: number_span.end,
                        };
                        let raw = unsafe { number.raw.get_unchecked(1..number.raw.len()) };
                        raw.parse()
                            .map_err(|_| Error {
                                kind: ErrorKind::InvalidNumber,
                                span: span.clone(),
                            })
                            .map(|value| ComponentValue::Number(Number { value, raw, span }))?
                    };
                    left = ComponentValue::SassBinaryExpression(SassBinaryExpression {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        span,
                    });
                    continue;
                }
                TokenWithSpan {
                    token: Token::Dimension(token),
                    span,
                } if precedence == PRECEDENCE_PLUS
                    && (token.value.raw.starts_with('+')
                        || token.value.raw.starts_with('-') && span.start == left.span().end) =>
                {
                    let (dimension, dimension_span) = expect!(self, Dimension);
                    let op = SassBinaryOperator {
                        kind: if dimension.value.raw.starts_with('+') {
                            SassBinaryOperatorKind::Plus
                        } else {
                            SassBinaryOperatorKind::Minus
                        },
                        span: Span {
                            start: dimension_span.start,
                            end: dimension_span.start + 1,
                        },
                    };
                    let span = Span {
                        start: left.span().start,
                        end: dimension_span.end,
                    };
                    let right = {
                        (
                            crate::token::Dimension {
                                value: crate::token::Number {
                                    raw: unsafe {
                                        dimension
                                            .value
                                            .raw
                                            .get_unchecked(1..dimension.value.raw.len())
                                    },
                                },
                                unit: dimension.unit,
                            },
                            Span {
                                start: dimension_span.start + 1,
                                end: dimension_span.end,
                            },
                        )
                            .try_into()
                            .map(ComponentValue::Dimension)?
                    };
                    left = ComponentValue::SassBinaryExpression(SassBinaryExpression {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        span,
                    });
                    continue;
                }
                _ => break,
            };

            let right = self.parse_sass_bin_expr_recursively(precedence + 1, allow_comparison)?;
            // delimiter can't be calculated
            if let ComponentValue::Delimiter(Delimiter { span, .. }) = right {
                return Err(Error {
                    kind: ErrorKind::ExpectSassExpression,
                    span,
                });
            }

            let span = Span {
                start: left.span().start,
                end: right.span().end,
            };
            left = ComponentValue::SassBinaryExpression(SassBinaryExpression {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_sass_flags(&mut self) -> PResult<(Vec<SassFlag<'s>>, Option<usize>)> {
        let mut flags: Vec<SassFlag<'s>> = Vec::with_capacity(1);
        let mut end = None;
        while let Some((_, exclamation_span)) = eat!(self, Exclamation) {
            let keyword = self.parse::<Ident>()?;
            let keyword_span = keyword.span.clone();
            util::assert_no_ws_or_comment(&exclamation_span, &keyword_span)?;
            end = Some(keyword_span.end);

            match &*keyword.name {
                "default" => {
                    if flags.iter().any(|flag| flag.keyword.name == "default") {
                        self.recoverable_errors.push(Error {
                            kind: ErrorKind::DuplicatedSassFlag("default"),
                            span: Span {
                                start: exclamation_span.start,
                                end: keyword.span.end,
                            },
                        });
                    }
                }
                "global" => {
                    if flags.iter().any(|flag| flag.keyword.name == "global") {
                        self.recoverable_errors.push(Error {
                            kind: ErrorKind::DuplicatedSassFlag("global"),
                            span: Span {
                                start: exclamation_span.start,
                                end: keyword.span.end,
                            },
                        });
                    }
                }
                _ => self.recoverable_errors.push(Error {
                    kind: ErrorKind::InvalidSassFlagName(keyword.name.to_string()),
                    span: keyword.span.clone(),
                }),
            }

            flags.push(SassFlag {
                keyword,
                span: Span {
                    start: exclamation_span.start,
                    end: keyword_span.end,
                },
            });
        }

        Ok((flags, end))
    }

    pub(super) fn parse_sass_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let (first, Span { start, mut end }) = match peek!(self) {
            TokenWithSpan {
                token: Token::Ident(..),
                ..
            } => {
                let (ident, ident_span) = expect!(self, Ident);
                (
                    SassInterpolatedIdentElement::Static((ident, ident_span.clone()).into()),
                    ident_span,
                )
            }
            TokenWithSpan {
                token: Token::HashLBrace(..),
                ..
            } => self.parse_sass_interpolated_ident_expr()?,
            TokenWithSpan { token, span } => {
                use crate::{
                    token::{HashLBrace, Ident},
                    tokenizer::TokenSymbol,
                };
                return Err(Error {
                    kind: ErrorKind::ExpectOneOf(
                        vec![Ident::symbol(), HashLBrace::symbol()],
                        token.symbol(),
                    ),
                    span: span.clone(),
                });
            }
        };

        let mut elements = self.parse_sass_interpolated_ident_rest(&mut end)?;
        if elements.is_empty() {
            if let SassInterpolatedIdentElement::Static(ident) = first {
                return Ok(InterpolableIdent::Literal(Ident {
                    name: ident.value,
                    raw: ident.raw,
                    span: ident.span,
                }));
            }
        }

        elements.insert(0, first);
        Ok(InterpolableIdent::SassInterpolated(SassInterpolatedIdent {
            elements,
            span: Span { start, end },
        }))
    }

    pub(super) fn parse_sass_interpolated_ident_rest(
        &mut self,
        end: &mut usize,
    ) -> PResult<Vec<SassInterpolatedIdentElement<'s>>> {
        let mut elements = vec![];
        loop {
            if let Some((token, span)) = self.tokenizer.scan_ident_template()? {
                *end = span.end;
                elements.push(SassInterpolatedIdentElement::Static((token, span).into()));
            } else if matches!(
                peek!(self),
                TokenWithSpan { token: Token::HashLBrace(..), span } if *end == span.start
            ) {
                let (element, span) = self.parse_sass_interpolated_ident_expr()?;
                *end = span.end;
                elements.push(element);
            } else {
                return Ok(elements);
            }
        }
    }

    fn parse_sass_interpolated_ident_expr(
        &mut self,
    ) -> PResult<(SassInterpolatedIdentElement<'s>, Span)> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(self, HashLBrace).1.start;
        let expr = self.parse_maybe_sass_list(/* allow_comma */ true)?;
        let end = expect!(self, RBrace).1.end;
        Ok((
            SassInterpolatedIdentElement::Expression(expr),
            Span { start, end },
        ))
    }

    fn parse_sass_invocation_args(&mut self) -> PResult<(Vec<ComponentValue<'s>>, Vec<Span>)> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let mut values = Vec::with_capacity(4);
        let mut comma_spans = vec![];
        while !matches!(peek!(self).token, Token::RParen(..) | Token::Eof(..)) {
            match peek!(self).token {
                Token::Comma(..) => {
                    let TokenWithSpan { span, .. } = bump!(self);
                    self.recoverable_errors.push(Error {
                        kind: ErrorKind::ExpectComponentValue,
                        span,
                    });
                    continue;
                }
                _ => {
                    let value = self.parse_maybe_sass_list(/* allow_comma */ false)?;
                    if let Some((_, span)) = eat!(self, DotDotDot) {
                        let span = Span {
                            start: value.span().start,
                            end: span.end,
                        };
                        values.push(ComponentValue::SassArbitraryArgument(
                            SassArbitraryArgument {
                                value: Box::new(value),
                                span,
                            },
                        ));
                    } else if let ComponentValue::SassVariable(sass_var) = value {
                        if let Some((_, colon_span)) = eat!(self, Colon) {
                            let value = self.parse_maybe_sass_list(/* allow_comma */ false)?;
                            let span = Span {
                                start: sass_var.span.start,
                                end: value.span().end,
                            };
                            values.push(ComponentValue::SassKeywordArgument(SassKeywordArgument {
                                name: sass_var,
                                colon_span,
                                value: Box::new(value),
                                span,
                            }));
                        } else {
                            values.push(ComponentValue::SassVariable(sass_var));
                        }
                    } else {
                        values.push(value);
                    }
                }
            }
            if !matches!(peek!(self).token, Token::RParen(..) | Token::Eof(..)) {
                comma_spans.push(expect!(self, Comma).1);
            }
        }
        debug_assert!(values.len() - comma_spans.len() <= 1);
        Ok((values, comma_spans))
    }

    fn parse_sass_module_config(
        &mut self,
        allow_overridable: bool,
    ) -> PResult<Option<SassModuleConfig<'s>>> {
        match &peek!(self).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("with") => {
                let TokenWithSpan {
                    span: with_span, ..
                } = bump!(self);
                let start = with_span.start;
                let end;
                let (_, lparen_span) = expect!(self, LParen);

                let mut items = vec![self.parse_sass_module_config_item(allow_overridable)?];
                let mut comma_spans = vec![];
                if let Some((_, span)) = eat!(self, RParen) {
                    end = span.end;
                } else {
                    comma_spans.push(expect!(self, Comma).1);
                    loop {
                        if let Some((_, span)) = eat!(self, RParen) {
                            end = span.end;
                            break;
                        }

                        items.push(self.parse_sass_module_config_item(allow_overridable)?);
                        if let Some((_, span)) = eat!(self, RParen) {
                            end = span.end;
                            break;
                        } else {
                            comma_spans.push(expect!(self, Comma).1);
                        }
                    }
                }
                debug_assert!(items.len() - comma_spans.len() <= 1);

                Ok(Some(SassModuleConfig {
                    with_span,
                    lparen_span,
                    items,
                    comma_spans,
                    span: Span { start, end },
                }))
            }
            _ => Ok(None),
        }
    }

    fn parse_sass_module_config_item(
        &mut self,
        allow_overridable: bool,
    ) -> PResult<SassModuleConfigItem<'s>> {
        let variable = self.parse::<SassVariable>()?;
        let (_, colon_span) = expect!(self, Colon);
        let value = self.parse_maybe_sass_list(/* allow_comma */ false)?;

        let (flags, end) = if allow_overridable {
            self.parse_sass_flags()
                .map(|(flags, end)| (flags, end.unwrap_or_else(|| value.span().end)))?
        } else {
            (vec![], value.span().end)
        };

        let span = Span {
            start: variable.span.start,
            end,
        };
        Ok(SassModuleConfigItem {
            variable,
            colon_span,
            value,
            flags,
            span,
        })
    }

    /// This method will consume `)` token.
    fn parse_sass_params(&mut self) -> PResult<SassParams<'s>> {
        let mut parameters = vec![];
        let mut arbitrary_parameter = None;
        let mut comma_spans = vec![];
        let end;
        loop {
            if let Some((_, span)) = eat!(self, RParen) {
                end = span.end;
                break;
            }

            let name = self.parse::<SassVariable>()?;
            let token_with_span = bump!(self);
            match token_with_span.token {
                Token::Comma(..) => {
                    let span = name.span.clone();
                    parameters.push(SassParameter {
                        name,
                        default_value: None,
                        span,
                    });
                    comma_spans.push(token_with_span.span);
                    continue;
                }
                Token::Colon(..) => {
                    let value = self.parse_maybe_sass_list(/* allow_comma */ false)?;
                    let end = value.span().end;
                    let default_value_span = Span {
                        start: token_with_span.span.start,
                        end,
                    };
                    let span = Span {
                        start: name.span.start,
                        end,
                    };
                    parameters.push(SassParameter {
                        name,
                        default_value: Some(SassParameterDefaultValue {
                            colon_span: token_with_span.span,
                            value,
                            span: default_value_span,
                        }),
                        span,
                    });
                }
                Token::DotDotDot(..) => {
                    let span = Span {
                        start: name.span().start,
                        end: token_with_span.span.end,
                    };
                    arbitrary_parameter = Some(SassArbitraryParameter { name, span });
                    if let Some((_, comma_span)) = eat!(self, Comma) {
                        comma_spans.push(comma_span);
                    }
                    end = expect!(self, RParen).1.end;
                    break;
                }
                Token::RParen(..) => {
                    let span = name.span.clone();
                    parameters.push(SassParameter {
                        name,
                        default_value: None,
                        span,
                    });
                    end = token_with_span.span.end;
                    break;
                }
                token => {
                    return Err(Error {
                        kind: ErrorKind::Unexpected(")", token.symbol()),
                        span: token_with_span.span,
                    });
                }
            }
            if let Some((_, span)) = eat!(self, RParen) {
                end = span.end;
                break;
            } else {
                comma_spans.push(expect!(self, Comma).1);
            }
        }

        debug_assert!(
            parameters.len() + arbitrary_parameter.iter().count() - comma_spans.len() <= 1
        );
        Ok((parameters, arbitrary_parameter, comma_spans, end))
    }

    pub(super) fn parse_sass_qualified_name(
        &mut self,
        module: Ident<'s>,
    ) -> PResult<SassQualifiedName<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let (_, dot_span) = expect!(self, Dot);
        let member = if let Token::DollarVar(..) = peek!(self).token {
            self.parse().map(SassModuleMemberName::Variable)?
        } else {
            self.parse().map(SassModuleMemberName::Ident)?
        };

        let expr_span = member.span();
        util::assert_no_ws_or_comment(&dot_span, expr_span)?;

        let span = Span {
            start: module.span.start,
            end: expr_span.end,
        };
        Ok(SassQualifiedName {
            module,
            member,
            span,
        })
    }

    fn parse_sass_unary_expression(&mut self) -> PResult<ComponentValue<'s>> {
        let op = match &peek!(self).token {
            Token::Plus(..) => SassUnaryOperator {
                kind: SassUnaryOperatorKind::Plus,
                span: bump!(self).span,
            },
            Token::Minus(..) => SassUnaryOperator {
                kind: SassUnaryOperatorKind::Minus,
                span: bump!(self).span,
            },
            Token::Ident(token) if token.raw == "not" => SassUnaryOperator {
                kind: SassUnaryOperatorKind::Not,
                span: bump!(self).span,
            },
            _ => return self.parse_component_value_atom(),
        };

        let expr = self.parse_sass_unary_expression()?;
        let span = Span {
            start: op.span.start,
            end: expr.span().end,
        };
        Ok(ComponentValue::SassUnaryExpression(SassUnaryExpression {
            expr: Box::new(expr),
            op,
            span,
        }))
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassAtRoot<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let kind = if matches!(peek!(input).token, Token::LParen(..)) {
            SassAtRootKind::Query(input.parse()?)
        } else {
            SassAtRootKind::Selector(input.parse()?)
        };

        let span = kind.span().clone();
        Ok(SassAtRoot { kind, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassAtRootQuery<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LParen).1.start;

        let modifier = {
            let (token, span) = expect!(input, Ident);
            let ident_name = token.name();
            if ident_name.eq_ignore_ascii_case("with") {
                SassAtRootQueryModifier {
                    kind: SassAtRootQueryModifierKind::With,
                    span,
                }
            } else if ident_name.eq_ignore_ascii_case("without") {
                SassAtRootQueryModifier {
                    kind: SassAtRootQueryModifierKind::Without,
                    span,
                }
            } else {
                return Err(Error {
                    kind: ErrorKind::ExpectSassAtRootWithOrWithout,
                    span,
                });
            }
        };
        let colon_span = expect!(input, Colon).1;

        let mut rules = Vec::with_capacity(1);
        loop {
            match &peek!(input).token {
                Token::Ident(..) | Token::HashLBrace(..) => {
                    rules.push(SassAtRootQueryRule::Ident(input.parse()?));
                }
                Token::Str(..) | Token::StrTemplate(..) => {
                    rules.push(SassAtRootQueryRule::Str(input.parse()?));
                }
                _ => break,
            }
        }
        let end = expect!(input, RParen).1.end;

        Ok(SassAtRootQuery {
            modifier,
            colon_span,
            rules,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassConditionalClause<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let condition = input.parse::<ComponentValue>()?;
        let block = input.parse::<SimpleBlock>()?;
        let span = Span {
            start: condition.span().start,
            end: block.span.end,
        };
        Ok(SassConditionalClause {
            condition,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassContent<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, LParen);
        let (args, comma_spans) = input.parse_sass_invocation_args()?;
        let (_, Span { end, .. }) = expect!(input, RParen);
        Ok(SassContent {
            args,
            comma_spans,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassEach<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let first_binding = input.parse::<SassVariable>()?;
        let start = first_binding.span().start;

        let mut bindings = vec![first_binding];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            bindings.push(input.parse()?);
        }
        debug_assert_eq!(comma_spans.len() + 1, bindings.len());

        let (keyword_in, keyword_in_span) = expect!(input, Ident);
        if keyword_in.name() != "in" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("in"),
                span: keyword_in_span,
            });
        }

        let expr = input.parse_maybe_sass_list(/* allow_comma */ true)?;
        let span = Span {
            start,
            end: expr.span().end,
        };
        Ok(SassEach {
            bindings,
            comma_spans,
            in_span: keyword_in_span,
            expr,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassExtend<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let selectors = input.parse::<CompoundSelectorList>()?;
        let start = selectors.span.start;
        let mut end = selectors.span.end;

        let optional = if let Some((_, exclamation_span)) = eat!(input, Exclamation) {
            let (keyword, keyword_span) = expect_without_ws_or_comments!(input, Ident);
            if keyword.name().eq_ignore_ascii_case("optional") {
                end = keyword_span.end;
                let span = Span {
                    start: exclamation_span.start,
                    end: keyword_span.end,
                };
                Some(SassFlag {
                    keyword: (keyword, keyword_span).into(),
                    span,
                })
            } else {
                input.recoverable_errors.push(Error {
                    kind: ErrorKind::ExpectSassKeyword("optional"),
                    span: keyword_span,
                });
                None
            }
        } else {
            None
        };

        Ok(SassExtend {
            selectors,
            optional,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassFor<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let binding = input.parse::<SassVariable>()?;

        let (keyword_from, keyword_from_span) = expect!(input, Ident);
        if keyword_from.name() != "from" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("from"),
                span: keyword_from_span,
            });
        }

        let start = input.parse()?;
        let boundary = input.parse()?;
        let end = input.parse::<ComponentValue>()?;

        let span = Span {
            start: binding.span.start,
            end: end.span().end,
        };
        Ok(SassFor {
            binding,
            from_span: keyword_from_span,
            start,
            end,
            boundary,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassForBoundary {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (keyword, span) = expect!(input, Ident);
        match &*keyword.name() {
            "to" => Ok(SassForBoundary {
                kind: SassForBoundaryKind::Exclusive,
                span,
            }),
            "through" => Ok(SassForBoundary {
                kind: SassForBoundaryKind::Inclusive,
                span,
            }),
            _ => Err(Error {
                kind: ErrorKind::ExpectSassKeyword("to"),
                span,
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassForward<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let path = input.parse::<InterpolableStr>()?;
        let mut span = path.span().clone();

        let prefix = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("as") => {
                let TokenWithSpan { span: as_span, .. } = bump!(input);
                let name = input.parse()?;
                let (_, Span { end, .. }) = expect_without_ws_or_comments!(input, Asterisk);
                let span = Span {
                    start: as_span.start,
                    end,
                };
                Some(SassForwardPrefix {
                    as_span,
                    name,
                    span,
                })
            }
            _ => None,
        };

        let visibility = if let TokenWithSpan {
            token: Token::Ident(keyword),
            span: keyword_span,
        } = peek!(input)
        {
            let start = keyword_span.start;
            let name = keyword.name();
            if name.eq_ignore_ascii_case("hide") {
                let keyword_span = bump!(input).span;
                let mut members = vec![];
                let mut comma_spans = vec![];
                loop {
                    match &peek!(input).token {
                        Token::Ident(..) => {
                            members.push(input.parse().map(SassForwardMember::Ident)?)
                        }
                        _ => members.push(input.parse().map(SassForwardMember::Variable)?),
                    }
                    if let Some((_, span)) = eat!(input, Comma) {
                        comma_spans.push(span);
                    } else {
                        break;
                    }
                }
                Some(SassForwardVisibility {
                    modifier: SassForwardVisibilityModifier {
                        kind: SassForwardVisibilityModifierKind::Hide,
                        span: keyword_span,
                    },
                    members,
                    comma_spans,
                    span: Span {
                        start,
                        end: input.tokenizer.current_offset(),
                    },
                })
            } else if name.eq_ignore_ascii_case("show") {
                let keyword_span = bump!(input).span;
                let mut members = vec![];
                let mut comma_spans = vec![];
                loop {
                    match &peek!(input).token {
                        Token::Ident(..) => {
                            members.push(input.parse().map(SassForwardMember::Ident)?)
                        }
                        _ => members.push(input.parse().map(SassForwardMember::Variable)?),
                    }
                    if let Some((_, span)) = eat!(input, Comma) {
                        comma_spans.push(span);
                    } else {
                        break;
                    }
                }
                Some(SassForwardVisibility {
                    modifier: SassForwardVisibilityModifier {
                        kind: SassForwardVisibilityModifierKind::Show,
                        span: keyword_span,
                    },
                    members,
                    comma_spans,
                    span: Span {
                        start,
                        end: input.tokenizer.current_offset(),
                    },
                })
            } else {
                None
            }
        } else {
            None
        };

        let config = input.parse_sass_module_config(/* allow_overridable */ true)?;
        if let Some(config) = &config {
            span.end = config.span.end;
        }

        Ok(SassForward {
            path,
            prefix,
            visibility,
            config,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassFunction<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let name = input.parse::<Ident>()?;
        let start = name.span.start;

        let parameters = input.parse::<SassParameters>()?;

        let span = Span {
            start,
            end: parameters.span.end,
        };
        Ok(SassFunction {
            name,
            parameters,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassIfAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(input, AtKeyword).1.start;

        let if_clause = input.parse::<SassConditionalClause>()?;
        let mut else_if_clauses = Vec::<SassConditionalClause>::new();
        let mut else_clause: Option<SimpleBlock> = None;
        let mut else_spans = vec![];

        while let Token::AtKeyword(at_keyword) = &peek!(input).token {
            match &*at_keyword.ident.name() {
                "else" => {
                    else_spans.push(bump!(input).span);
                    match &peek!(input).token {
                        Token::Ident(ident) if ident.name() == "if" => {
                            bump!(input);
                            else_if_clauses.push(input.parse()?);
                        }
                        _ => {
                            else_clause = Some(input.parse()?);
                            break;
                        }
                    }
                }
                // `elseif` is deprecated by Sass
                "elseif" => {
                    else_spans.push(bump!(input).span);
                    else_if_clauses.push(input.parse()?);
                }
                _ => break,
            }
        }

        debug_assert_eq!(
            else_spans.len(),
            else_if_clauses.len() + else_clause.iter().count()
        );
        let span = Span {
            start,
            end: else_clause
                .as_ref()
                .map(|else_clause| else_clause.span.end)
                .or_else(|| {
                    else_if_clauses
                        .last()
                        .map(|else_if_clause| else_if_clause.span.end)
                })
                .unwrap_or(if_clause.span.end),
        };
        Ok(SassIfAtRule {
            if_clause,
            else_if_clauses,
            else_clause,
            else_spans,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassImportPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<Str>()?;
        let mut span = first.span.clone();

        let mut paths = vec![first];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            paths.push(input.parse()?);
        }
        debug_assert_eq!(comma_spans.len() + 1, paths.len());

        if let Some(Str {
            span: Span { end, .. },
            ..
        }) = paths.last()
        {
            span.end = *end;
        }
        Ok(SassImportPrelude {
            paths,
            comma_spans,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassInclude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let name = input.parse::<FunctionName>()?;
        let mut span = name.span().clone();

        let arguments = if matches!(peek!(input).token, Token::LParen(..)) {
            let arguments = input.parse::<SassIncludeArgs>()?;
            span.end = arguments.span.end;
            Some(arguments)
        } else {
            None
        };

        let content_block_params = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("using") => {
                let content_block_params = input.parse::<SassIncludeContentBlockParams>()?;
                span.end = content_block_params.span.end;
                Some(content_block_params)
            }
            _ => None,
        };

        Ok(SassInclude {
            name,
            arguments,
            content_block_params,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassIncludeArgs<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, LParen);
        let (args, comma_spans) = input.parse_sass_invocation_args()?;
        let (_, Span { end, .. }) = expect!(input, RParen);
        Ok(SassIncludeArgs {
            args,
            comma_spans,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassIncludeContentBlockParams<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match bump!(input) {
            TokenWithSpan {
                token: Token::Ident(ident),
                span: using_span,
            } if ident.name().eq_ignore_ascii_case("using") => {
                let params = input.parse::<SassParameters>()?;
                let span = Span {
                    start: using_span.start,
                    end: params.span.end,
                };
                Ok(SassIncludeContentBlockParams {
                    using_span,
                    params,
                    span,
                })
            }
            TokenWithSpan { span, .. } => Err(Error {
                kind: ErrorKind::ExpectSassKeyword("using"),
                span,
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassInterpolatedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (first, first_span) = expect!(input, StrTemplate);
        let quote = first.raw.chars().next().unwrap();
        debug_assert!(quote == '\'' || quote == '"');
        let mut span = first_span.clone();
        let mut elements = vec![SassInterpolatedStrElement::Static(
            (first, first_span).into(),
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let (token, str_tpl_span) = input.tokenizer.scan_string_template(quote)?;
                let tail = token.tail;
                let end = str_tpl_span.end;
                elements.push(SassInterpolatedStrElement::Static(
                    (token, str_tpl_span).into(),
                ));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '#' is consumed, so '{' left only
                expect!(input, LBrace);
                elements.push(SassInterpolatedStrElement::Expression(
                    input.parse_maybe_sass_list(/* allow_comma */ true)?,
                ));
                expect!(input, RBrace);
            }
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(SassInterpolatedStr { elements, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassInterpolatedUrl<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let (first, first_span) = match input.tokenizer.scan_url_raw_or_template()? {
            TokenWithSpan {
                token: Token::UrlTemplate(template),
                span,
            } => (template, span),
            TokenWithSpan { token, span } => {
                return Err(Error {
                    kind: ErrorKind::Unexpected("<url template>", token.symbol()),
                    span,
                });
            }
        };
        let mut span = first_span.clone();
        let mut elements = vec![SassInterpolatedUrlElement::Static(
            (first, first_span).into(),
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let (token, url_tpl_span @ Span { end, .. }) =
                    input.tokenizer.scan_url_template()?;
                let tail = token.tail;
                elements.push(SassInterpolatedUrlElement::Static(
                    (token, url_tpl_span).into(),
                ));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '#' is consumed, so '{' left only
                expect!(input, LBrace);
                elements.push(SassInterpolatedUrlElement::Expression(
                    input.parse_maybe_sass_list(/* allow_comma */ true)?,
                ));
                expect!(input, RBrace);
            }
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(SassInterpolatedUrl { elements, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if let ComponentValue::SassList(list) =
            input.parse_maybe_sass_list(/* allow_comma */ true)?
        {
            Ok(list)
        } else {
            use crate::{token::Comma, tokenizer::TokenSymbol};
            let TokenWithSpan { token, span } = bump!(input);
            Err(Error {
                kind: ErrorKind::Unexpected(Comma::symbol(), token.symbol()),
                span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassMap<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LParen).1.start;

        let mut items = vec![];
        let mut comma_spans = vec![];
        while !matches!(&peek!(input).token, Token::RParen(..)) {
            items.push(input.parse()?);
            if !matches!(&peek!(input).token, Token::RParen(..)) {
                comma_spans.push(expect!(input, Comma).1);
            }
        }
        debug_assert!(items.len() - comma_spans.len() <= 1);

        let end = expect!(input, RParen).1.end;
        Ok(SassMap {
            items,
            comma_spans,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassMapItem<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let key = input.parse_maybe_sass_list(/* allow_comma */ false)?;
        let (_, colon_span) = expect!(input, Colon);
        let value = input.parse_maybe_sass_list(/* allow_comma */ false)?;
        let span = Span {
            start: key.span().start,
            end: value.span().end,
        };
        Ok(SassMapItem {
            key,
            colon_span,
            value,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassMixin<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let name = input.parse::<Ident>()?;
        let start = name.span.start;
        let mut end = name.span.end;

        let parameters = if matches!(peek!(input).token, Token::LParen(..)) {
            let parameters = input.parse::<SassParameters>()?;
            end = parameters.span.end;
            Some(parameters)
        } else {
            None
        };

        Ok(SassMixin {
            name,
            parameters,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassParameters<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, LParen);
        let (params, arbitrary_param, comma_spans, end) = input.parse_sass_params()?;
        Ok(SassParameters {
            params,
            arbitrary_param,
            comma_spans,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassNestingDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let block = input.parse::<SimpleBlock>()?;
        let span = block.span.clone();

        Ok(SassNestingDeclaration { block, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassParenthesizedExpression<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LParen).1.start;
        let expr = Box::new(
            input
                .with_state(ParserState {
                    sass_ctx: input.state.sass_ctx | SASS_CTX_ALLOW_DIV,
                    ..input.state.clone()
                })
                .parse_maybe_sass_list(/* allow_comma */ true)?,
        );
        let end = expect!(input, RParen).1.end;
        Ok(SassParenthesizedExpression {
            expr,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassPlaceholderSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, percent_span) = expect!(input, Percent);
        let name = input.parse::<InterpolableIdent>()?;
        let name_span = name.span();
        util::assert_no_ws_or_comment(&percent_span, name_span)?;
        let span = Span {
            start: percent_span.start,
            end: name_span.end,
        };
        Ok(SassPlaceholderSelector { name, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassUse<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let path = input.parse::<InterpolableStr>()?;
        let mut span = path.span().clone();

        let namespace = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("as") => {
                let namespace = input.parse::<SassUseNamespace>()?;
                span.end = namespace.span.end;
                Some(namespace)
            }
            _ => None,
        };

        let config = input.parse_sass_module_config(/* allow_overridable */ false)?;
        if let Some(config) = &config {
            span.end = config.span.end;
        }

        Ok(SassUse {
            path,
            namespace,
            config,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassUseNamespace<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let as_span = match peek!(input) {
            TokenWithSpan {
                token: Token::Ident(ident),
                ..
            } if ident.name().eq_ignore_ascii_case("as") => bump!(input).span,
            TokenWithSpan { span, .. } => {
                return Err(Error {
                    kind: ErrorKind::ExpectSassKeyword("as"),
                    span: span.clone(),
                })
            }
        };
        match bump!(input) {
            TokenWithSpan {
                token: Token::Asterisk(..),
                span: asterisk_span,
            } => {
                let span = Span {
                    start: as_span.start,
                    end: asterisk_span.end,
                };
                Ok(SassUseNamespace {
                    as_span,
                    kind: SassUseNamespaceKind::Unnamed(SassUnnamedNamespace {
                        span: asterisk_span,
                    }),
                    span,
                })
            }
            TokenWithSpan {
                token: Token::Ident(ident),
                span: ident_span,
            } => {
                let span = Span {
                    start: as_span.start,
                    end: ident_span.end,
                };
                Ok(SassUseNamespace {
                    as_span,
                    kind: SassUseNamespaceKind::Named((ident, ident_span).into()),
                    span,
                })
            }
            TokenWithSpan { span, .. } => Err(Error {
                kind: ErrorKind::ExpectSassUseNamespace,
                span,
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let (dollar_var, span) = expect!(input, DollarVar);
        Ok(SassVariable {
            name: (
                dollar_var.ident,
                Span {
                    start: span.start + 1,
                    end: span.end,
                },
            )
                .into(),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassVariableDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let namespace = if let Some((ident_token, span)) = eat!(input, Ident) {
            let (_, dot_span) = expect!(input, Dot);
            util::assert_no_ws_or_comment(&span, &dot_span)?;
            let TokenWithSpan {
                span: next_span, ..
            } = peek!(input);
            util::assert_no_ws_or_comment(&dot_span, next_span)?;
            Some(Ident::from((ident_token, span)))
        } else {
            None
        };

        let name = input.parse::<SassVariable>()?;
        let (_, colon_span) = expect!(input, Colon);
        let value = input
            .with_state(ParserState {
                sass_ctx: input.state.sass_ctx | SASS_CTX_ALLOW_DIV,
                ..input.state.clone()
            })
            .parse_maybe_sass_list(/* allow_comma */ true)?;

        let (flags, end) = input.parse_sass_flags()?;

        let span = Span {
            start: namespace
                .as_ref()
                .map(|namespace| namespace.span.start)
                .unwrap_or(name.span.start),
            end: end.unwrap_or_else(|| value.span().end),
        };

        if namespace.is_some() && flags.iter().any(|flag| flag.keyword.name == "global") {
            input.recoverable_errors.push(Error {
                kind: ErrorKind::UnexpectedSassFlag("global"),
                span: span.clone(),
            });
        }

        Ok(SassVariableDeclaration {
            namespace,
            name,
            colon_span,
            value,
            flags,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for UnknownSassAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let (_, at_span) = expect!(input, At);
        let name = input.parse_sass_interpolated_ident()?;
        let name_span = name.span();
        util::assert_no_ws_or_comment(&at_span, name_span)?;

        let (prelude, block, end) = input.parse_unknown_at_rule()?;
        let span = Span {
            start: at_span.start,
            end: end.unwrap_or(name_span.end),
        };
        Ok(UnknownSassAtRule {
            name,
            prelude,
            block,
            span,
        })
    }
}
