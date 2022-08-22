use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    eat,
    error::{Error, ErrorKind, PResult},
    expect,
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
            "each" if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                Ok(Some(Statement::SassEachAtRule(self.parse()?)))
            }
            "for" if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                Ok(Some(Statement::SassForAtRule(self.parse()?)))
            }
            "while" if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                Ok(Some(Statement::SassWhileAtRule(self.parse()?)))
            }
            "warn" if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                Ok(Some(Statement::SassWarnAtRule(self.parse()?)))
            }
            "error" if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                Ok(Some(Statement::SassErrorAtRule(self.parse()?)))
            }
            "debug" if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                Ok(Some(Statement::SassDebugAtRule(self.parse()?)))
            }
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
            let operator = match self.tokenizer.peek()? {
                Token::Asterisk(token) if precedence == PRECEDENCE_MULTIPLY => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::Multiply,
                        span: token.span,
                    }
                }
                Token::Percent(token) if precedence == PRECEDENCE_MULTIPLY => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::Modulo,
                        span: token.span,
                    }
                }
                Token::Plus(token) if precedence == PRECEDENCE_PLUS => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::Plus,
                        span: token.span,
                    }
                }
                Token::Minus(token) if precedence == PRECEDENCE_PLUS => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::Minus,
                        span: token.span,
                    }
                }
                Token::GreaterThan(token) if precedence == PRECEDENCE_RELATIONAL => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::GreaterThan,
                        span: token.span,
                    }
                }
                Token::GreaterThanEqual(token) if precedence == PRECEDENCE_RELATIONAL => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::GreaterThanEqual,
                        span: token.span,
                    }
                }
                Token::LessThan(token) if precedence == PRECEDENCE_RELATIONAL => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::LessThan,
                        span: token.span,
                    }
                }
                Token::LessThanEqual(token) if precedence == PRECEDENCE_RELATIONAL => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::LessThanEqual,
                        span: token.span,
                    }
                }
                Token::EqualEqual(token) if precedence == PRECEDENCE_EQUALITY => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::EqualsEquals,
                        span: token.span,
                    }
                }
                Token::ExclamationEqual(token) if precedence == PRECEDENCE_EQUALITY => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::ExclamationEquals,
                        span: token.span,
                    }
                }
                Token::Ident(token) if token.raw == "and" && precedence == PRECEDENCE_AND => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::And,
                        span: token.span,
                    }
                }
                Token::Ident(token) if token.raw == "or" && precedence == PRECEDENCE_OR => {
                    self.tokenizer.bump()?;
                    BinaryOperator {
                        kind: BinaryOperatorKind::Or,
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
        let (first, mut span) = match self.tokenizer.peek()? {
            Token::Ident(ident) => {
                self.tokenizer.bump()?;
                let span = ident.span.clone();
                match self.tokenizer.peek()? {
                    Token::HashLBrace(token) if ident.span.end == token.span.start => {
                        (SassInterpolatedIdentElement::Static(ident.into()), span)
                    }
                    _ => return Ok(InterpolableIdent::Literal(ident.into())),
                }
            }
            Token::HashLBrace(..) => self.parse_sass_interpolated_ident_expr()?,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected("<ident> or '#{'", token.symbol()),
                    span: token.span().clone(),
                })
            }
        };
        let mut last_span_end = span.end;

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match self.tokenizer.peek()? {
                Token::Ident(token) if last_span_end == token.span.start => {
                    last_span_end = token.span.end;
                    self.tokenizer.bump()?;
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
        let expr = self.parse_component_values(/* allow_comma */ true)?;
        let r_brace = expect!(self, RBrace);
        Ok((
            SassInterpolatedIdentElement::Expression(expr),
            Span {
                start: hash_lbrace.span.start,
                end: r_brace.span.end,
            },
        ))
    }

    fn parse_sass_unary_expression(&mut self) -> PResult<ComponentValue<'s>> {
        let op = match self.tokenizer.peek()? {
            Token::Plus(token) => {
                let _ = self.tokenizer.bump();
                SassUnaryOperator {
                    kind: SassUnaryOperatorKind::Plus,
                    span: token.span,
                }
            }
            Token::Minus(token) => {
                let _ = self.tokenizer.bump();
                SassUnaryOperator {
                    kind: SassUnaryOperatorKind::Minus,
                    span: token.span,
                }
            }
            Token::Ident(token) if token.raw == "not" => {
                let _ = self.tokenizer.bump();
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassDebugAtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token = expect!(input, AtKeyword);
        debug_assert_eq!(&*token.ident.name, "debug");
        let expr = input.parse_component_values(/* allow_comma */ true)?;
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
        let expr = input.parse_component_values(/* allow_comma */ true)?;
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassInterpolatedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = expect!(input, StrTemplate);
        let quote = first.raw.chars().next().unwrap();
        debug_assert!(quote == '\'' || quote == '"');
        let mut span = first.span.clone();
        let mut elements = vec![SassInterpolatedStrElement::Static(
            InterpolableStrStaticPart {
                value: first.value,
                raw: first.raw,
                span: first.span,
            },
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let token = input.tokenizer.scan_string_template(quote)?;
                let end = token.span.end;
                elements.push(SassInterpolatedStrElement::Static(
                    InterpolableStrStaticPart {
                        value: token.value,
                        raw: token.raw,
                        span: token.span,
                    },
                ));
                if token.tail {
                    span.end = end;
                    break;
                }
            } else {
                // '#' is consumed, so '{' left only
                expect!(input, LBrace);
                elements.push(SassInterpolatedStrElement::Expression(
                    input.parse_component_values(/* allow_comma */ true)?,
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
        let mut elements = vec![SassInterpolatedUrlElement::Static(
            InterpolableUrlStaticPart {
                value: first.value,
                raw: first.raw,
                span: first.span,
            },
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let token = input.tokenizer.scan_url_template()?;
                let end = token.span.end;
                elements.push(SassInterpolatedUrlElement::Static(
                    InterpolableUrlStaticPart {
                        value: token.value,
                        raw: token.raw,
                        span: token.span,
                    },
                ));
                if token.tail {
                    span.end = end;
                    break;
                }
            } else {
                // '#' is consumed, so '{' left only
                expect!(input, LBrace);
                elements.push(SassInterpolatedUrlElement::Expression(
                    input.parse_component_values(/* allow_comma */ true)?,
                ));
                expect!(input, RBrace);
            }
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(SassInterpolatedUrl { elements, span })
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
        let name = input.parse::<InterpolableIdent>()?;
        let span = Span {
            start: percent.span.start,
            end: name.span().end,
        };
        Ok(SassPlaceholderSelector { name, span })
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
        let value = input.parse_component_values(/* allow_comma */ true)?;

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
        let expr = input.parse_component_values(/* allow_comma */ true)?;
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
