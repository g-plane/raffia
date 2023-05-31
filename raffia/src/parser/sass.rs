use super::{state::ParserState, Parser};
use crate::{
    ast::*,
    bump,
    config::Syntax,
    eat,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    Parse,
};

const PRECEDENCE_MULTIPLY: u8 = 6;
const PRECEDENCE_PLUS: u8 = 5;
const PRECEDENCE_RELATIONAL: u8 = 4;
const PRECEDENCE_EQUALITY: u8 = 3;
const PRECEDENCE_AND: u8 = 2;
const PRECEDENCE_OR: u8 = 1;

const FLAG_DEFAULT: u8 = 1;
const FLAG_GLOBAL: u8 = 2;

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn parse_maybe_sass_list(&mut self, allow_comma: bool) -> PResult<ComponentValue<'s>> {
        let single_value = if allow_comma {
            self.parse_maybe_sass_list(false)?
        } else {
            self.parse_sass_bin_expr()?
        };

        let mut items = vec![];
        let mut separator = SassListSeparatorKind::Unknown;
        let mut end = single_value.span().end;
        loop {
            match peek!(self) {
                TokenWithSpan {
                    token:
                        Token::LBrace(..)
                        | Token::RBrace(..)
                        | Token::RParen(..)
                        | Token::Semicolon(..)
                        | Token::Colon(..)
                        | Token::Dedent(..)
                        | Token::Linebreak(..)
                        | Token::Exclamation(..)
                        | Token::DotDotDot(..)
                        | Token::Eof(..),
                    ..
                } => break,
                TokenWithSpan {
                    token: Token::Comma(..),
                    ..
                } => {
                    if !allow_comma {
                        break;
                    }
                    match separator {
                        SassListSeparatorKind::Unknown => separator = SassListSeparatorKind::Comma,
                        SassListSeparatorKind::Comma => {}
                        SassListSeparatorKind::Space => break,
                    }
                    bump!(self);
                }
                TokenWithSpan { span, .. } => {
                    if end < span.start && matches!(separator, SassListSeparatorKind::Unknown) {
                        separator = SassListSeparatorKind::Space;
                    }
                    let item = if allow_comma {
                        self.parse_maybe_sass_list(false)?
                    } else {
                        self.parse_sass_bin_expr()?
                    };
                    end = item.span().end;
                    items.push(item);
                }
            }
        }

        if items.is_empty() && !matches!(separator, SassListSeparatorKind::Comma) {
            // If there is a trailing comma it can be a Sass list,
            // though there is only one element.
            Ok(single_value)
        } else {
            debug_assert_ne!(separator, SassListSeparatorKind::Unknown);

            let span = Span {
                start: single_value.span().start,
                end,
            };
            items.insert(0, single_value);
            Ok(ComponentValue::SassList(SassList {
                items,
                has_parens: false,
                separator,
                span,
            }))
        }
    }

    pub(super) fn parse_sass_at_rule(
        &mut self,
        at_keyword_name: &str,
    ) -> PResult<Option<(Statement<'s>, bool)>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));
        match at_keyword_name {
            "each" => Ok(Some((Statement::SassEachAtRule(self.parse()?), true))),
            "for" => Ok(Some((Statement::SassForAtRule(self.parse()?), true))),
            "if" => Ok(Some((Statement::SassIfAtRule(self.parse()?), true))),
            "while" => Ok(Some((Statement::SassWhileAtRule(self.parse()?), true))),
            "mixin" => Ok(Some((Statement::SassMixinAtRule(self.parse()?), true))),
            "include" => {
                let at_rule = self.parse::<SassIncludeAtRule>()?;
                let is_block = at_rule.block.is_some();
                Ok(Some((Statement::SassIncludeAtRule(at_rule), is_block)))
            }
            "content" => Ok(Some((Statement::SassContentAtRule(self.parse()?), false))),
            "use" => Ok(Some((Statement::SassUseAtRule(self.parse()?), false))),
            "function" => Ok(Some((
                Statement::SassFunctionAtRule(
                    self.with_state(ParserState {
                        in_sass_function: true,
                        ..self.state.clone()
                    })
                    .parse()?,
                ),
                true,
            ))),
            "return" => {
                let rule = self.parse::<SassReturnAtRule>()?;
                if !self.state.in_sass_function {
                    self.recoverable_errors.push(Error {
                        kind: ErrorKind::ReturnOutsideFunction,
                        span: rule.span.clone(),
                    });
                }
                Ok(Some((Statement::SassReturnAtRule(rule), false)))
            }
            "extend" => Ok(Some((Statement::SassExtendAtRule(self.parse()?), false))),
            "forward" => Ok(Some((Statement::SassForwardAtRule(self.parse()?), false))),
            "at-root" => Ok(Some((Statement::SassAtRootAtRule(self.parse()?), true))),
            "warn" => Ok(Some((Statement::SassWarnAtRule(self.parse()?), false))),
            "error" => Ok(Some((Statement::SassErrorAtRule(self.parse()?), false))),
            "debug" => Ok(Some((Statement::SassDebugAtRule(self.parse()?), false))),
            "else" => Err(Error {
                kind: ErrorKind::UnexpectedSassElseAtRule,
                span: bump!(self).span,
            }),
            _ => Ok(None),
        }
    }

    pub(super) fn parse_sass_bin_expr(&mut self) -> PResult<ComponentValue<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));
        self.parse_sass_bin_expr_recursively(0)
    }

    fn parse_sass_bin_expr_recursively(&mut self, precedence: u8) -> PResult<ComponentValue<'s>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            self.parse_sass_unary_expression()?
        } else {
            self.parse_sass_bin_expr_recursively(precedence + 1)?
        };

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
                } if precedence == PRECEDENCE_RELATIONAL => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::GreaterThan,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::GreaterThanEqual(..),
                    ..
                } if precedence == PRECEDENCE_RELATIONAL => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::GreaterThanOrEqual,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::LessThan(..),
                    ..
                } if precedence == PRECEDENCE_RELATIONAL => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::LessThan,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::LessThanEqual(..),
                    ..
                } if precedence == PRECEDENCE_RELATIONAL => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::LessThanOrEqual,
                    span: bump!(self).span,
                },
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

            let right = self.parse_sass_bin_expr_recursively(precedence + 1)?;
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

    fn parse_sass_flags_into_bits(&mut self) -> PResult<(u8, usize)> {
        let mut flags = 0;
        let mut end = self.tokenizer.current_offset();
        while let Some((_, exclamation_span)) = eat!(self, Exclamation) {
            let keyword = self.parse::<Ident>()?;
            self.assert_no_ws_or_comment(&exclamation_span, &keyword.span)?;
            end = keyword.span.end;

            match &*keyword.name {
                "default" => flags |= FLAG_DEFAULT,
                "global" => flags |= FLAG_GLOBAL,
                _ => self.recoverable_errors.push(Error {
                    kind: ErrorKind::InvalidSassFlagName(keyword.name.to_string()),
                    span: keyword.span,
                }),
            }
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
                return Err(Error {
                    kind: ErrorKind::Unexpected("<ident>` or `#{", token.symbol()),
                    span: span.clone(),
                })
            }
        };

        let mut elements = vec![];
        loop {
            if let Some((token, span)) = self.tokenizer.scan_ident_template()? {
                end = span.end;
                elements.push(SassInterpolatedIdentElement::Static((token, span).into()));
            } else if matches!(
                peek!(self),
                TokenWithSpan { token: Token::HashLBrace(..), span } if end == span.start
            ) {
                let (element, span) = self.parse_sass_interpolated_ident_expr()?;
                end = span.end;
                elements.push(element);
            } else {
                break;
            }
        }

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

    fn parse_sass_invocation_args(&mut self) -> PResult<Vec<ComponentValue<'s>>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let mut values = Vec::with_capacity(4);
        while !matches!(peek!(self).token, Token::RParen(..) | Token::Eof(..)) {
            match peek!(self).token {
                Token::Exclamation(..) => {
                    // while this syntax is weird, Bootstrap is actually using it
                    values.push(self.parse().map(ComponentValue::ImportantAnnotation)?);
                }
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
                        if eat!(self, Colon).is_some() {
                            let value = self.parse_maybe_sass_list(/* allow_comma */ false)?;
                            let span = Span {
                                start: sass_var.span.start,
                                end: value.span().end,
                            };
                            values.push(ComponentValue::SassKeywordArgument(SassKeywordArgument {
                                name: sass_var,
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
                expect!(self, Comma);
            }
        }
        Ok(values)
    }

    fn parse_sass_module_config(
        &mut self,
        allow_overridable: bool,
    ) -> PResult<Option<Vec<SassModuleConfigItem<'s>>>> {
        match &peek!(self).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("with") => {
                bump!(self);
                expect!(self, LParen);
                let mut config = vec![self.parse_sass_module_config_item(allow_overridable)?];
                if !matches!(&peek!(self).token, Token::RParen(..)) {
                    expect!(self, Comma);
                }

                while eat!(self, RParen).is_none() {
                    config.push(self.parse_sass_module_config_item(allow_overridable)?);
                    if eat!(self, RParen).is_some() {
                        break;
                    } else {
                        expect!(self, Comma);
                    }
                }
                Ok(Some(config))
            }
            _ => Ok(None),
        }
    }

    fn parse_sass_module_config_item(
        &mut self,
        allow_overridable: bool,
    ) -> PResult<SassModuleConfigItem<'s>> {
        let variable = self.parse::<SassVariable>()?;
        expect!(self, Colon);
        let value = self.parse_component_values(/* allow_comma */ false)?;

        let important = self.try_parse(ImportantAnnotation::parse).ok();

        let (overridable, end) = if allow_overridable {
            let (flags, end) = self.parse_sass_flags_into_bits()?;
            (flags & FLAG_DEFAULT != 0, end)
        } else {
            (
                false,
                important
                    .as_ref()
                    .map(|important| important.span.end)
                    .unwrap_or(value.span.end),
            )
        };

        let span = Span {
            start: variable.span.start,
            end,
        };
        Ok(SassModuleConfigItem {
            variable,
            value,
            important,
            overridable,
            span,
        })
    }

    /// This method will consume `)` token.
    fn parse_sass_params(
        &mut self,
    ) -> PResult<(Vec<SassParameter<'s>>, Option<SassArbitraryParameter<'s>>)> {
        let mut parameters = vec![];
        let mut arbitrary_parameter = None;
        while eat!(self, RParen).is_none() {
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
                    continue;
                }
                Token::Colon(..) => {
                    let default_value = self.parse_maybe_sass_list(/* allow_comma */ false)?;
                    let span = Span {
                        start: name.span.start,
                        end: default_value.span().end,
                    };
                    parameters.push(SassParameter {
                        name,
                        default_value: Some(default_value),
                        span,
                    });
                }
                Token::DotDotDot(..) => {
                    let span = Span {
                        start: name.span().start,
                        end: token_with_span.span.end,
                    };
                    arbitrary_parameter = Some(SassArbitraryParameter { name, span });
                    eat!(self, Comma);
                    expect!(self, RParen);
                    break;
                }
                Token::RParen(..) => {
                    let span = name.span.clone();
                    parameters.push(SassParameter {
                        name,
                        default_value: None,
                        span,
                    });
                    break;
                }
                token => {
                    return Err(Error {
                        kind: ErrorKind::Unexpected(")", token.symbol()),
                        span: token_with_span.span,
                    });
                }
            }
            if eat!(self, RParen).is_some() {
                break;
            } else {
                expect!(self, Comma);
            }
        }

        Ok((parameters, arbitrary_parameter))
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
        self.assert_no_ws_or_comment(&dot_span, expr_span)?;

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

        let expr = self.parse_component_value_atom()?;
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassAtRootAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let mut selector = None;
        let mut query = None;
        let block: SimpleBlock;

        match &peek!(input).token {
            Token::LBrace(..) => block = input.parse()?,
            Token::LParen(..) => {
                query = Some(input.parse()?);
                block = input.parse()?;
            }
            _ => {
                selector = Some(input.parse()?);
                block = input.parse()?;
            }
        }

        let span = Span {
            start,
            end: block.span.end,
        };
        Ok(SassAtRootAtRule {
            selector,
            query,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassAtRootQuery<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LParen).1.start;

        let kind = {
            let (token, span) = expect!(input, Ident);
            let ident_name = token.name();
            if ident_name.eq_ignore_ascii_case("with") {
                SassAtRootQueryKind::With
            } else if ident_name.eq_ignore_ascii_case("without") {
                SassAtRootQueryKind::Without
            } else {
                return Err(Error {
                    kind: ErrorKind::ExpectSassAtRootWithOrWithout,
                    span,
                });
            }
        };
        expect!(input, Colon);

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
            kind,
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassContentAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, span) = expect!(input, AtKeyword);
        let mut end = span.end;

        let arguments = if eat!(input, LParen).is_some() {
            let arguments = input.parse_sass_invocation_args()?;
            end = expect!(input, RParen).1.end;
            Some(arguments)
        } else {
            None
        };
        Ok(SassContentAtRule {
            arguments,
            span: Span {
                start: span.start,
                end,
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassDebugAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let expr = input.parse_component_values(/* allow_comma */ true)?;
        let span = Span {
            start,
            end: expr.span.end,
        };
        Ok(SassDebugAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassEachAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;

        let mut bindings = vec![input.parse()?];
        while eat!(input, Comma).is_some() {
            bindings.push(input.parse()?);
        }

        let (keyword_in, keyword_in_span) = expect!(input, Ident);
        if keyword_in.name() != "in" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("in"),
                span: keyword_in_span,
            });
        }

        let expr = input.parse_maybe_sass_list(/* allow_comma */ true)?;
        let body = input.parse::<SimpleBlock>()?;
        let span = Span {
            start,
            end: body.span.end,
        };
        Ok(SassEachAtRule {
            bindings,
            expr,
            body,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassErrorAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let expr = input.parse_component_values(/* allow_comma */ true)?;
        let span = Span {
            start,
            end: expr.span.end,
        };
        Ok(SassErrorAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassExtendAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;

        let selectors = input.parse()?;

        let optional = if let Some((_, exclamation_span)) = eat!(input, Exclamation) {
            let (keyword, keyword_span) = expect_without_ws_or_comments!(input, Ident);
            if keyword.name().eq_ignore_ascii_case("optional") {
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

        let end = input.tokenizer.current_offset();
        Ok(SassExtendAtRule {
            selectors,
            optional,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassForAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let at_keyword_span = expect!(input, AtKeyword).1;

        let binding = input.parse()?;

        let (keyword_from, keyword_from_span) = expect!(input, Ident);
        if keyword_from.name() != "from" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("from"),
                span: keyword_from_span,
            });
        }
        let start = input.parse()?;

        let (keyword_to_or_through, keyword_to_or_through_span) = expect!(input, Ident);
        let keyword_to_or_through_name = keyword_to_or_through.name();
        if keyword_to_or_through_name != "to" && keyword_to_or_through_name != "through" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("to"),
                span: keyword_to_or_through_span,
            });
        }
        let is_exclusive = keyword_to_or_through_name == "to";
        let end = input.parse()?;

        let body = input.parse::<SimpleBlock>()?;
        let span = Span {
            start: at_keyword_span.start,
            end: body.span.end,
        };
        Ok(SassForAtRule {
            binding,
            start,
            end,
            is_exclusive,
            body,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassForwardAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(input, AtKeyword).1.start;

        let path = input.parse()?;

        let prefix = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("as") => {
                bump!(input);
                let prefix = input.parse()?;
                expect_without_ws_or_comments!(input, Asterisk);
                Some(prefix)
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
                bump!(input);
                let mut members = vec![];
                loop {
                    match &peek!(input).token {
                        Token::Ident(..) => {
                            members.push(input.parse().map(SassForwardMember::Ident)?)
                        }
                        _ => members.push(input.parse().map(SassForwardMember::Variable)?),
                    }
                    if eat!(input, Comma).is_none() {
                        break;
                    }
                }
                Some(SassForwardVisibility {
                    kind: SassForwardVisibilityKind::Hide,
                    members,
                    span: Span {
                        start,
                        end: input.tokenizer.current_offset(),
                    },
                })
            } else if name.eq_ignore_ascii_case("show") {
                bump!(input);
                let mut members = vec![];
                loop {
                    match &peek!(input).token {
                        Token::Ident(..) => {
                            members.push(input.parse().map(SassForwardMember::Ident)?)
                        }
                        _ => members.push(input.parse().map(SassForwardMember::Variable)?),
                    }
                    if eat!(input, Comma).is_none() {
                        break;
                    }
                }
                Some(SassForwardVisibility {
                    kind: SassForwardVisibilityKind::Show,
                    members,
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

        Ok(SassForwardAtRule {
            path,
            prefix,
            visibility,
            config,
            span: Span {
                start,
                end: input.tokenizer.current_offset(),
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassFunctionAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(input, AtKeyword).1.start;

        let name = input.parse()?;

        expect!(input, LParen);
        let (parameters, arbitrary_parameter) = input.parse_sass_params()?;

        let body = input.parse::<SimpleBlock>()?;

        let span = Span {
            start,
            end: body.span.end,
        };
        Ok(SassFunctionAtRule {
            name,
            parameters,
            arbitrary_parameter,
            body,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassIfAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(input, AtKeyword).1.start;

        let if_clause = input.parse()?;
        let mut else_if_clauses = vec![];
        let mut else_clause = None;

        while let Token::AtKeyword(at_keyword) = &peek!(input).token {
            match &*at_keyword.ident.name() {
                "else" => {
                    bump!(input);
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
                    bump!(input);
                    else_if_clauses.push(input.parse()?);
                }
                _ => break,
            }
        }

        Ok(SassIfAtRule {
            if_clause,
            else_if_clauses,
            else_clause,
            span: Span {
                start,
                end: input.tokenizer.current_offset(),
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassIncludeAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(input, AtKeyword).1.start;

        let name = input.parse()?;

        let arguments = if eat!(input, LParen).is_some() {
            let arguments = input.parse_sass_invocation_args()?;
            expect!(input, RParen);
            Some(arguments)
        } else {
            None
        };

        let (content_block_params, content_block_arbitrary_params) = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("using") => {
                bump!(input);
                expect!(input, LParen);
                let (params, arbitrary_param) = input.parse_sass_params()?;
                (Some(params), arbitrary_param)
            }
            _ => (None, None),
        };

        let block = match &peek!(input).token {
            Token::LBrace(..) => input.parse().map(Some)?,
            _ => None,
        };

        Ok(SassIncludeAtRule {
            name,
            arguments,
            content_block_params,
            content_block_arbitrary_param: content_block_arbitrary_params,
            block,
            span: Span {
                start,
                end: input.tokenizer.current_offset(),
            },
        })
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
        let paren_start = eat!(input, LParen).map(|(_, span)| span.start);

        let mut list = if let ComponentValue::SassList(list) =
            input.parse_maybe_sass_list(/* allow_comma */ true)?
        {
            list
        } else {
            use crate::{
                token::{Comma, RParen},
                tokenizer::TokenSymbol,
            };
            let TokenWithSpan { token, span } = bump!(input);
            return Err(Error {
                kind: ErrorKind::Unexpected(
                    if paren_start.is_some() {
                        RParen::symbol()
                    } else {
                        Comma::symbol()
                    },
                    token.symbol(),
                ),
                span,
            });
        };

        if let Some(start) = paren_start {
            list.span = Span {
                start,
                end: expect!(input, RParen).1.end,
            };
            list.has_parens = true;
        }

        Ok(list)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassMap<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LParen).1.start;

        let mut items = vec![];
        while !matches!(&peek!(input).token, Token::RParen(..)) {
            items.push(input.parse()?);
            if !matches!(&peek!(input).token, Token::RParen(..)) {
                expect!(input, Comma);
            }
        }

        let end = expect!(input, RParen).1.end;
        Ok(SassMap {
            items,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassMapItem<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let key = input.parse_maybe_sass_list(/* allow_comma */ false)?;
        expect!(input, Colon);
        let value = input.parse_maybe_sass_list(/* allow_comma */ false)?;
        let span = Span {
            start: key.span().start,
            end: value.span().end,
        };
        Ok(SassMapItem { key, value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassMixinAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(input, AtKeyword).1.start;

        let name = input.parse()?;

        let mut parameters = None;
        let mut arbitrary_parameter = None;
        if eat!(input, LParen).is_some() {
            let (params, arbitrary_param) = input.parse_sass_params()?;
            parameters = Some(params);
            arbitrary_parameter = arbitrary_param;
        }

        let body = input.parse::<SimpleBlock>()?;

        let span = Span {
            start,
            end: body.span.end,
        };
        Ok(SassMixinAtRule {
            name,
            parameters,
            arbitrary_parameter,
            body,
            span,
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
        let expr = Box::new(input.parse()?);
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
        input.assert_no_ws_or_comment(&percent_span, name_span)?;
        let span = Span {
            start: percent_span.start,
            end: name_span.end,
        };
        Ok(SassPlaceholderSelector { name, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassReturnAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let expr = input.parse_component_values(/* allow_comma */ true)?;
        let span = Span {
            start,
            end: expr.span.end,
        };
        Ok(SassReturnAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassUseAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;

        let path = input.parse()?;
        let namespace = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("as") => {
                bump!(input);
                input.parse().map(Some)?
            }
            _ => None,
        };

        let config = input.parse_sass_module_config(/* allow_overridable */ false)?;

        Ok(SassUseAtRule {
            path,
            namespace,
            config,
            span: Span {
                start,
                end: input.tokenizer.current_offset(),
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassUseNamespace<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token_with_span = bump!(input);
        match token_with_span.token {
            Token::Asterisk(..) => Ok(SassUseNamespace::Unnamed(SassUnnamedNamespace {
                span: token_with_span.span,
            })),
            Token::Ident(ident) => Ok(SassUseNamespace::Named(
                (ident, token_with_span.span).into(),
            )),
            _ => Err(Error {
                kind: ErrorKind::ExpectSassUseNamespace,
                span: token_with_span.span,
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
            input.assert_no_ws_or_comment(&span, &dot_span)?;
            let TokenWithSpan {
                span: next_span, ..
            } = peek!(input);
            input.assert_no_ws_or_comment(&dot_span, next_span)?;
            Some(Ident::from((ident_token, span)))
        } else {
            None
        };

        let name = input.parse::<SassVariable>()?;
        expect!(input, Colon);
        let value = input.parse_maybe_sass_list(/* allow_comma */ true)?;

        let important = input.try_parse(ImportantAnnotation::parse).ok();

        let (flags, end) = input.parse_sass_flags_into_bits()?;

        let span = Span {
            start: namespace
                .as_ref()
                .map(|namespace| namespace.span.start)
                .unwrap_or(name.span.start),
            end,
        };

        let force_global = flags & FLAG_GLOBAL != 0;
        if namespace.is_some() && force_global {
            input.recoverable_errors.push(Error {
                kind: ErrorKind::UnexpectedSassFlag("global"),
                span: span.clone(),
            });
        }

        Ok(SassVariableDeclaration {
            namespace,
            name,
            value,
            important,
            overridable: flags & FLAG_DEFAULT != 0,
            force_global,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassWarnAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let expr = input.parse_component_values(/* allow_comma */ true)?;
        let span = Span {
            start,
            end: expr.span.end,
        };
        Ok(SassWarnAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassWhileAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let condition = input.parse()?;
        let body = input.parse::<SimpleBlock>()?;
        let span = Span {
            start,
            end: body.span.end,
        };
        Ok(SassWhileAtRule {
            condition,
            body,
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
        input.assert_no_ws_or_comment(&at_span, name_span)?;

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
