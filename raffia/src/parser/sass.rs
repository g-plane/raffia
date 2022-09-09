use super::{state::ParserState, Parser};
use crate::{
    ast::*,
    bump,
    config::Syntax,
    eat,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::Token,
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
            let operator = match peek!(self) {
                Token::Asterisk(..) if precedence == PRECEDENCE_MULTIPLY => {
                    let token = expect!(self, Asterisk);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Multiply,
                        span: token.span,
                    }
                }
                Token::Percent(..) if precedence == PRECEDENCE_MULTIPLY => {
                    let token = expect!(self, Percent);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Modulo,
                        span: token.span,
                    }
                }
                Token::Plus(..) if precedence == PRECEDENCE_PLUS => {
                    let token = expect!(self, Plus);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Plus,
                        span: token.span,
                    }
                }
                Token::Minus(..) if precedence == PRECEDENCE_PLUS => {
                    let token = expect!(self, Minus);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Minus,
                        span: token.span,
                    }
                }
                Token::GreaterThan(..) if precedence == PRECEDENCE_RELATIONAL => {
                    let token = expect!(self, GreaterThan);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::GreaterThan,
                        span: token.span,
                    }
                }
                Token::GreaterThanEqual(..) if precedence == PRECEDENCE_RELATIONAL => {
                    let token = expect!(self, GreaterThanEqual);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::GreaterThanOrEqual,
                        span: token.span,
                    }
                }
                Token::LessThan(..) if precedence == PRECEDENCE_RELATIONAL => {
                    let token = expect!(self, LessThan);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::LessThan,
                        span: token.span,
                    }
                }
                Token::LessThanEqual(..) if precedence == PRECEDENCE_RELATIONAL => {
                    let token = expect!(self, LessThanEqual);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::LessThanOrEqual,
                        span: token.span,
                    }
                }
                Token::EqualEqual(..) if precedence == PRECEDENCE_EQUALITY => {
                    let token = expect!(self, EqualEqual);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::EqualsEquals,
                        span: token.span,
                    }
                }
                Token::ExclamationEqual(..) if precedence == PRECEDENCE_EQUALITY => {
                    let token = expect!(self, ExclamationEqual);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::ExclamationEquals,
                        span: token.span,
                    }
                }
                Token::Ident(token) if token.raw == "and" && precedence == PRECEDENCE_AND => {
                    let token = expect!(self, Ident);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::And,
                        span: token.span,
                    }
                }
                Token::Ident(token) if token.raw == "or" && precedence == PRECEDENCE_OR => {
                    let token = expect!(self, Ident);
                    SassBinaryOperator {
                        kind: SassBinaryOperatorKind::Or,
                        span: token.span,
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
            Token::Ident(..) => {
                let ident = expect!(self, Ident);
                let span = ident.span.clone();
                match peek!(self) {
                    Token::HashLBrace(token) if ident.span.end == token.span.start => {
                        (SassInterpolatedIdentElement::Static(ident.into()), span)
                    }
                    _ => return Ok(InterpolableIdent::Literal(ident.into())),
                }
            }
            Token::HashLBrace(..) => self.parse_sass_interpolated_ident_expr()?,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected("<ident>` or `#{", token.symbol()),
                    span: token.span().clone(),
                })
            }
        };
        let mut last_span_end = span.end;

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match peek!(self) {
                Token::Ident(token) if last_span_end == token.span.start => {
                    last_span_end = token.span.end;
                    let token = expect!(self, Ident);
                    elements.push(SassInterpolatedIdentElement::Static(token.into()));
                }
                Token::HashLBrace(token) if last_span_end == token.span.start => {
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

        let hash_lbrace = expect!(self, HashLBrace);
        let expr = self.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
        let r_brace = expect!(self, RBrace);
        Ok((
            SassInterpolatedIdentElement::Expression(expr),
            Span {
                start: hash_lbrace.span.start,
                end: r_brace.span.end,
            },
        ))
    }

    pub(super) fn parse_sass_namespaced_expression(
        &mut self,
        namespace: Ident<'s>,
    ) -> PResult<SassNamespacedExpression<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let dot = expect!(self, Dot);
        let expr = match peek!(self) {
            Token::DollarVar(..) => self.parse().map(ComponentValue::SassVariable)?,
            _ => {
                let name = InterpolableIdent::Literal(expect!(self, Ident).into());
                self.parse_function(name).map(ComponentValue::Function)?
            }
        };

        let expr_span = expr.span();
        self.assert_no_ws_or_comment(&dot.span, expr_span)?;

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

    fn parse_sass_unary_expression(&mut self) -> PResult<ComponentValue<'s>> {
        let op = match peek!(self) {
            Token::Plus(..) => {
                let token = expect!(self, Plus);
                SassUnaryOperator {
                    kind: SassUnaryOperatorKind::Plus,
                    span: token.span,
                }
            }
            Token::Minus(..) => {
                let token = expect!(self, Minus);
                SassUnaryOperator {
                    kind: SassUnaryOperatorKind::Minus,
                    span: token.span,
                }
            }
            Token::Ident(token) if token.raw == "not" => {
                let token = expect!(self, Ident);
                SassUnaryOperator {
                    kind: SassUnaryOperatorKind::Not,
                    span: token.span,
                }
            }
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassDebugAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token = expect!(input, AtKeyword);
        debug_assert_eq!(&*token.ident.name, "debug");
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
        let span = Span {
            start: token.span.start,
            end: expr.span.end,
        };
        Ok(SassDebugAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassEachAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let at_keyword = expect!(input, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "each");

        let mut bindings = vec![input.parse()?];
        while eat!(input, Comma).is_some() {
            bindings.push(input.parse()?);
        }

        let keyword_in = expect!(input, Ident);
        if keyword_in.name != "in" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("in"),
                span: keyword_in.span,
            });
        }

        let expr = input.parse()?;
        let body = input.parse::<SimpleBlock>()?;
        let span = Span {
            start: at_keyword.span.start,
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
        let token = expect!(input, AtKeyword);
        debug_assert_eq!(&*token.ident.name, "error");
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
        let span = Span {
            start: token.span.start,
            end: expr.span.end,
        };
        Ok(SassErrorAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassForAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let at_keyword = expect!(input, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "for");

        let binding = input.parse()?;

        let keyword_from = expect!(input, Ident);
        if keyword_from.name != "from" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("from"),
                span: keyword_from.span,
            });
        }
        let start = input.parse()?;

        let keyword_to_or_through = expect!(input, Ident);
        if keyword_to_or_through.name != "to" && keyword_to_or_through.name != "through" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("to"),
                span: keyword_from.span,
            });
        }
        let is_exclusive = keyword_to_or_through.name == "to";
        let end = input.parse()?;

        let body = input.parse::<SimpleBlock>()?;
        let span = Span {
            start: at_keyword.span.start,
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

        let at_keyword = expect!(input, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "function");

        let name = input.parse()?;

        expect!(input, LParen);
        let mut parameters = vec![];
        let mut arbitrary_parameter = None;
        while eat!(input, RParen).is_none() {
            let name = input.parse::<SassVariable>()?;
            match bump!(input) {
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
                    let default_value = input.parse::<ComponentValue>()?;
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
                Token::DotDotDot(token) => {
                    let span = Span {
                        start: name.span().start,
                        end: token.span.end,
                    };
                    arbitrary_parameter = Some(SassArbitraryParameter { name, span });
                    eat!(input, Comma);
                    expect!(input, RParen);
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
                        span: token.span().clone(),
                    });
                }
            }
            if eat!(input, RParen).is_some() {
                break;
            } else {
                expect!(input, Comma);
            }
        }

        let body = input.parse::<SimpleBlock>()?;

        let span = Span {
            start: at_keyword.span.start,
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

        let at_keyword = expect!(input, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "if");

        let if_clause = input.parse()?;
        let mut else_if_clauses = vec![];
        let mut else_clause = None;

        while let Token::AtKeyword(at_keyword) = peek!(input) {
            if at_keyword.ident.name == "else" {
                bump!(input);
                match peek!(input) {
                    Token::Ident(ident) if ident.name == "if" => {
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
                start: at_keyword.span.start,
                end: input.tokenizer.current_offset(),
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassInterpolatedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = expect!(input, StrTemplate);
        let quote = first.raw.chars().next().unwrap();
        debug_assert!(quote == '\'' || quote == '"');
        let mut span = first.span.clone();
        let mut elements = vec![SassInterpolatedStrElement::Static(first.try_into()?)];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let token = input.tokenizer.scan_string_template(quote)?;
                let tail = token.tail;
                let end = token.span.end;
                elements.push(SassInterpolatedStrElement::Static(token.try_into()?));
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

        let first = match input.tokenizer.scan_url_raw_or_template()? {
            Token::UrlTemplate(template) => template,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected("<url template>", token.symbol()),
                    span: token.span().clone(),
                });
            }
        };
        let mut span = first.span.clone();
        let mut elements = vec![SassInterpolatedUrlElement::Static(first.try_into()?)];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let token = input.tokenizer.scan_url_template()?;
                let tail = token.tail;
                let end = token.span.end;
                elements.push(SassInterpolatedUrlElement::Static(token.try_into()?));
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassNestingDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = if input.syntax == Syntax::Scss {
            expect!(input, LBrace).span.start
        } else {
            expect!(input, Indent).span.start
        };

        let mut decls = Vec::with_capacity(3);
        let end;
        loop {
            match peek!(input) {
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
            match peek!(input) {
                Token::RBrace(..) if input.syntax == Syntax::Scss => {
                    end = expect!(input, RBrace).span.end;
                    break;
                }
                Token::Dedent(..) if input.syntax == Syntax::Sass => {
                    end = expect!(input, Dedent).span.end;
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
        let l_paren = expect!(input, LParen);
        let expr = Box::new(input.parse()?);
        let r_paren = expect!(input, RParen);
        Ok(SassParenthesizedExpression {
            expr,
            span: Span {
                start: l_paren.span.start,
                end: r_paren.span.end,
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassPlaceholderSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let percent = expect!(input, Percent);
        let name: Ident = expect_without_ws_or_comments!(input, Ident).into();
        let span = Span {
            start: percent.span.start,
            end: name.span.end,
        };
        Ok(SassPlaceholderSelector {
            name: InterpolableIdent::Literal(name),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassReturnAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token = expect!(input, AtKeyword);
        debug_assert_eq!(&*token.ident.name, "return");
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
        let span = Span {
            start: token.span.start,
            end: expr.span.end,
        };
        Ok(SassReturnAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassUseAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let at_keyword = expect!(input, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "use");

        let path = input.parse()?;
        let namespace = match peek!(input) {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("as") => {
                bump!(input);
                input.parse().map(Some)?
            }
            _ => None,
        };

        let mut config = vec![];
        match peek!(input) {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("with") => {
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
                start: at_keyword.span.start,
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
        match bump!(input) {
            Token::Asterisk(asterisk) => Ok(SassUseNamespace::Unnamed(SassUnnamedNamespace {
                span: asterisk.span,
            })),
            Token::Ident(ident) => Ok(SassUseNamespace::Named(ident.into())),
            token => Err(Error {
                kind: ErrorKind::ExpectSassUseNamespace,
                span: token.span().clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert!(matches!(input.syntax, Syntax::Scss | Syntax::Sass));

        let dollar_var = expect!(input, DollarVar);
        Ok(SassVariable {
            name: dollar_var.ident.into(),
            span: dollar_var.span,
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
        let token = expect!(input, AtKeyword);
        debug_assert_eq!(&*token.ident.name, "warn");
        let expr = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;
        let span = Span {
            start: token.span.start,
            end: expr.span.end,
        };
        Ok(SassWarnAtRule { expr, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassWhileAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let at_keyword = expect!(input, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "while");
        let condition = input.parse()?;
        let body = input.parse::<SimpleBlock>()?;
        let span = Span {
            start: at_keyword.span.start,
            end: body.span.end,
        };
        Ok(SassWhileAtRule {
            condition,
            body,
            span,
        })
    }
}
