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

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_sass_at_rule(
        &mut self,
        at_keyword_name: &str,
    ) -> PResult<Option<Statement<'s>>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));
        match at_keyword_name {
            "each" => Ok(Some(Statement::SassEachAtRule(self.parse()?))),
            "for" => Ok(Some(Statement::SassForAtRule(self.parse()?))),
            "if" => Ok(Some(Statement::SassIfAtRule(self.parse()?))),
            "while" => Ok(Some(Statement::SassWhileAtRule(self.parse()?))),
            "mixin" => Ok(Some(Statement::SassMixinAtRule(self.parse()?))),
            "include" => Ok(Some(Statement::SassIncludeAtRule(self.parse()?))),
            "content" => Ok(Some(Statement::SassContentAtRule(self.parse()?))),
            "use" => Ok(Some(Statement::SassUseAtRule(self.parse()?))),
            "function" => Ok(Some(Statement::SassFunctionAtRule(
                self.with_state(ParserState {
                    in_sass_function: true,
                    ..self.state.clone()
                })
                .parse()?,
            ))),
            "return" => {
                let rule = self.parse::<SassReturnAtRule>()?;
                if !self.state.in_sass_function {
                    self.recoverable_errors.push(Error {
                        kind: ErrorKind::ReturnOutsideFunction,
                        span: rule.span.clone(),
                    });
                }
                Ok(Some(Statement::SassReturnAtRule(rule)))
            }
            "extend" => Ok(Some(Statement::SassExtendAtRule(self.parse()?))),
            "warn" => Ok(Some(Statement::SassWarnAtRule(self.parse()?))),
            "error" => Ok(Some(Statement::SassErrorAtRule(self.parse()?))),
            "debug" => Ok(Some(Statement::SassDebugAtRule(self.parse()?))),
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
            let operator = match &peek!(self).token {
                Token::Asterisk(..) if precedence == PRECEDENCE_MULTIPLY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Multiply,
                    span: bump!(self).span,
                },
                Token::Percent(..) if precedence == PRECEDENCE_MULTIPLY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Modulo,
                    span: bump!(self).span,
                },
                Token::Plus(..) if precedence == PRECEDENCE_PLUS => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Plus,
                    span: bump!(self).span,
                },
                Token::Minus(..) if precedence == PRECEDENCE_PLUS => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::Minus,
                    span: bump!(self).span,
                },
                Token::GreaterThan(..) if precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::GreaterThan,
                        span: bump!(self).span,
                    }
                }
                Token::GreaterThanEqual(..) if precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::GreaterThanOrEqual,
                        span: bump!(self).span,
                    }
                }
                Token::LessThan(..) if precedence == PRECEDENCE_RELATIONAL => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::LessThan,
                    span: bump!(self).span,
                },
                Token::LessThanEqual(..) if precedence == PRECEDENCE_RELATIONAL => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::LessThanOrEqual,
                        span: bump!(self).span,
                    }
                }
                Token::EqualEqual(..) if precedence == PRECEDENCE_EQUALITY => SassBinaryOperator {
                    kind: SassBinaryOperatorKind::EqualsEquals,
                    span: bump!(self).span,
                },
                Token::ExclamationEqual(..) if precedence == PRECEDENCE_EQUALITY => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::ExclamationEquals,
                        span: bump!(self).span,
                    }
                }
                Token::Ident(token) if token.raw == "and" && precedence == PRECEDENCE_AND => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::And,
                        span: bump!(self).span,
                    }
                }
                Token::Ident(token) if token.raw == "or" && precedence == PRECEDENCE_OR => {
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Or,
                        span: bump!(self).span,
                    }
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

    pub(super) fn parse_sass_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));
        let (first, mut span) = match peek!(self) {
            TokenWithSpan {
                token: Token::Ident(..),
                ..
            } => {
                let (ident, ident_span) = expect!(self, Ident);
                match peek!(self) {
                    TokenWithSpan {
                        token: Token::HashLBrace(..),
                        span,
                    } if ident_span.end == span.start => (
                        SassInterpolatedIdentElement::Static(
                            InterpolableIdentStaticPart::from_token(ident, ident_span.clone()),
                        ),
                        ident_span,
                    ),
                    _ => {
                        return Ok(InterpolableIdent::Literal(Ident::from_token(
                            ident, ident_span,
                        )))
                    }
                }
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
        let mut last_span_end = span.end;

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match peek!(self) {
                TokenWithSpan {
                    token: Token::Ident(..),
                    span,
                } if last_span_end == span.start => {
                    let (token, span) = expect!(self, Ident);
                    last_span_end = span.end;
                    elements.push(SassInterpolatedIdentElement::Static(
                        InterpolableIdentStaticPart::from_token(token, span),
                    ));
                }
                TokenWithSpan {
                    token: Token::HashLBrace(..),
                    span,
                } if last_span_end == span.start => {
                    let (element, span) = self.parse_sass_interpolated_ident_expr()?;
                    elements.push(element);
                    last_span_end = span.end;
                }
                _ => break,
            }
        }

        span.end = last_span_end;
        Ok(InterpolableIdent::SassInterpolated(SassInterpolatedIdent {
            elements,
            span,
        }))
    }

    pub(super) fn parse_sass_interpolated_ident_expr(
        &mut self,
    ) -> PResult<(SassInterpolatedIdentElement<'s>, Span)> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let start = expect!(self, HashLBrace).1.start;
        let expr = self.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
        let end = expect!(self, RBrace).1.end;
        Ok((
            SassInterpolatedIdentElement::Expression(expr),
            Span { start, end },
        ))
    }

    pub(super) fn parse_sass_namespaced_expression(
        &mut self,
        namespace: Ident<'s>,
    ) -> PResult<SassNamespacedExpression<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let (_, dot_span) = expect!(self, Dot);
        let expr = match &peek!(self).token {
            Token::DollarVar(..) => self.parse().map(ComponentValue::SassVariable)?,
            _ => {
                let (ident, ident_span) = expect!(self, Ident);
                let name = InterpolableIdent::Literal(Ident::from_token(ident, ident_span));
                self.parse_function(name).map(ComponentValue::Function)?
            }
        };

        let expr_span = expr.span();
        self.assert_no_ws_or_comment(&dot_span, expr_span)?;

        let span = Span {
            start: namespace.span.start,
            end: expr_span.end,
        };
        Ok(SassNamespacedExpression {
            namespace,
            expr: Box::new(expr),
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
                    let default_value = self.parse::<ComponentValue>()?;
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassArbitraryArgument<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let name = input.parse::<SassVariable>()?;
        let (_, Span { end, .. }) = expect!(input, DotDotDot);
        let span = Span {
            start: name.span.start,
            end,
        };
        Ok(SassArbitraryArgument { name, span })
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
            let arguments = input
                .parse_component_values(
                    /* allow_comma */ false, /* allow_semicolon */ false,
                )?
                .values;
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
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
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

        let expr = input.parse()?;
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
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
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
        let selector = input.parse::<SelectorList>()?;
        let mut end = selector.span.end;

        let optional = if let Some((_, exclamation_span)) = eat!(input, Exclamation) {
            let (keyword, keyword_span) = expect_without_ws_or_comments!(input, Ident);
            if keyword.name().eq_ignore_ascii_case("optional") {
                let span = Span {
                    start: exclamation_span.start,
                    end: keyword_span.end,
                };
                end = keyword_span.end;
                Some(SassFlag {
                    keyword: Ident::from_token(keyword, keyword_span),
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

        Ok(SassExtendAtRule {
            selector,
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
            if at_keyword.ident.name() == "else" {
                bump!(input);
                match &peek!(input).token {
                    Token::Ident(ident) if ident.name() == "if" => {
                        bump!(input);
                        else_if_clauses.push(input.parse()?);
                    }
                    _ => else_clause = Some(input.parse()?),
                }
            } else {
                break;
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

        let mut arguments = None;
        let mut arbitrary_argument = None;
        if eat!(input, LParen).is_some() {
            let mut args = vec![];
            while eat!(input, RParen).is_none() {
                match &peek!(input).token {
                    Token::DollarVar(..) => {
                        if let Ok(arg) = input.try_parse(|parser| {
                            let name = parser.parse::<SassVariable>()?;
                            expect!(parser, Colon);
                            let value = parser.parse::<ComponentValue>()?;
                            let span = Span {
                                start: name.span.start,
                                end: value.span().end,
                            };
                            Ok(SassIncludeAtRuleArgument {
                                name: Some(name),
                                value,
                                span,
                            })
                        }) {
                            args.push(arg);
                        } else if let Ok(arbitrary_arg) =
                            input.try_parse(SassArbitraryArgument::parse)
                        {
                            arbitrary_argument = Some(arbitrary_arg);
                            expect!(input, RParen);
                            break;
                        } else {
                            let value = input.parse::<ComponentValue>()?;
                            let span = value.span().clone();
                            args.push(SassIncludeAtRuleArgument {
                                name: None,
                                value,
                                span,
                            });
                        }
                    }
                    _ => {
                        let value = input.parse::<ComponentValue>()?;
                        let span = value.span().clone();
                        args.push(SassIncludeAtRuleArgument {
                            name: None,
                            value,
                            span,
                        });
                    }
                }
                if eat!(input, RParen).is_some() {
                    break;
                } else {
                    expect!(input, Comma);
                }
            }
            arguments = Some(args);
        }

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
            arbitrary_argument,
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
            InterpolableStrStaticPart::from_token(first, first_span),
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let (token, str_tpl_span) = input.tokenizer.scan_string_template(quote)?;
                let tail = token.tail;
                let end = str_tpl_span.end;
                elements.push(SassInterpolatedStrElement::Static(
                    InterpolableStrStaticPart::from_token(token, str_tpl_span),
                ));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '#' is consumed, so '{' left only
                expect!(input, LBrace);
                elements.push(SassInterpolatedStrElement::Expression(
                    input.parse_component_values(
                        /* allow_comma */ true, /* allow_semicolon */ false,
                    )?,
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
            InterpolableUrlStaticPart::from_token(first, first_span),
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let (token, url_tpl_span @ Span { end, .. }) =
                    input.tokenizer.scan_url_template()?;
                let tail = token.tail;
                elements.push(SassInterpolatedUrlElement::Static(
                    InterpolableUrlStaticPart::from_token(token, url_tpl_span),
                ));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '#' is consumed, so '{' left only
                expect!(input, LBrace);
                elements.push(SassInterpolatedUrlElement::Expression(
                    input.parse_component_values(
                        /* allow_comma */ true, /* allow_semicolon */ false,
                    )?,
                ));
                expect!(input, RBrace);
            }
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(SassInterpolatedUrl { elements, span })
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
        let start = if input.syntax == Syntax::Scss {
            expect!(input, LBrace).1.start
        } else {
            expect!(input, Indent).1.start
        };

        let mut decls = Vec::with_capacity(3);
        let end;
        loop {
            match &peek!(input).token {
                Token::RBrace(..) if input.syntax == Syntax::Scss => {}
                Token::Dedent(..) if input.syntax == Syntax::Sass => {}
                _ => {
                    if input.syntax == Syntax::Scss {
                        expect!(input, Semicolon);
                        while eat!(input, Semicolon).is_some() {}
                    } else {
                        expect!(input, Linebreak);
                        while eat!(input, Linebreak).is_some() {}
                    }
                }
            }
            match &peek!(input).token {
                Token::RBrace(..) if input.syntax == Syntax::Scss => {
                    end = bump!(input).span.end;
                    break;
                }
                Token::Dedent(..) if input.syntax == Syntax::Sass => {
                    end = bump!(input).span.end;
                    break;
                }
                _ => decls.push(input.parse::<Declaration>()?),
            }
        }

        Ok(SassNestingDeclaration {
            decls,
            span: Span { start, end },
        })
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
        let start = expect!(input, Percent).1.start;
        let (name, name_span) = expect_without_ws_or_comments!(input, Ident);
        let span = Span {
            start,
            end: name_span.end,
        };
        Ok(SassPlaceholderSelector {
            name: InterpolableIdent::Literal(Ident::from_token(name, name_span)),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassReturnAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
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

        let mut config = vec![];
        match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("with") => {
                bump!(input);
                expect!(input, LParen);
                loop {
                    config.push(input.parse::<SassUseConfigItem>()?);
                    if eat!(input, RParen).is_some() {
                        break;
                    } else {
                        expect!(input, Comma);
                    }
                }
            }
            _ => {}
        }

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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassUseConfigItem<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let variable = input.parse::<SassVariable>()?;
        expect!(input, Colon);
        let value = input.parse::<ComponentValue>()?;
        let span = Span {
            start: variable.span.start,
            end: value.span().end,
        };
        Ok(SassUseConfigItem {
            variable,
            value,
            span,
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
            Token::Ident(ident) => Ok(SassUseNamespace::Named(Ident::from_token(
                ident,
                token_with_span.span,
            ))),
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
            name: Ident::from_token(
                dollar_var.ident,
                Span {
                    start: span.start + 1,
                    end: span.end,
                },
            ),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassVariableDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let name = input.parse::<SassVariable>()?;
        expect!(input, Colon);
        let value = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(SassVariableDeclaration { name, value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassWarnAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, AtKeyword).1.start;
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
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
