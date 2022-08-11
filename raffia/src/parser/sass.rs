use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    eat,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
};

const PRECEDENCE_MULTIPLY: u8 = 6;
const PRECEDENCE_PLUS: u8 = 5;
const PRECEDENCE_RELATIONAL: u8 = 4;
const PRECEDENCE_EQUALITY: u8 = 3;
const PRECEDENCE_AND: u8 = 2;
const PRECEDENCE_OR: u8 = 1;

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
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

    pub(super) fn parse_sass_each_at_rule(&mut self) -> PResult<SassEachAtRule<'s>> {
        let at_keyword = expect!(self, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "each");

        let mut bindings = vec![self.parse_sass_variable()?];
        while eat!(self, Comma).is_some() {
            bindings.push(self.parse_sass_variable()?);
        }

        let keyword_in = expect!(self, Ident);
        if keyword_in.name != "in" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("in"),
                span: keyword_in.span,
            });
        }

        let expr = self.parse_component_value()?;
        let body = self.parse_simple_block()?;
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

    pub(super) fn parse_sass_for_at_rule(&mut self) -> PResult<SassForAtRule<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let at_keyword = expect!(self, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "for");

        let binding = self.parse_sass_variable()?;

        let keyword_from = expect!(self, Ident);
        if keyword_from.name != "from" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("from"),
                span: keyword_from.span,
            });
        }
        let start = self.parse_component_value()?;

        let keyword_to_or_through = expect!(self, Ident);
        if keyword_to_or_through.name != "to" && keyword_to_or_through.name != "through" {
            return Err(Error {
                kind: ErrorKind::ExpectSassKeyword("to"),
                span: keyword_from.span,
            });
        }
        let is_exclusive = keyword_to_or_through.name == "to";
        let end = self.parse_component_value()?;

        let body = self.parse_simple_block()?;
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

    pub(super) fn parse_sass_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
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

    pub(super) fn parse_sass_interpolated_str(&mut self) -> PResult<SassInterpolatedStr<'s>> {
        let first = expect!(self, StrTemplate);
        let mut span = first.span.clone();
        let mut elements = vec![SassInterpolatedStrElement::Static(
            InterpolableStrStaticPart {
                value: first.value,
                raw: first.raw,
                span: first.span,
            },
        )];

        loop {
            match self.tokenizer.bump()? {
                Token::StrTemplate(token) => {
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
                }
                // '#' is consumed, so '{' left only
                Token::LBrace(..) => {
                    elements.push(SassInterpolatedStrElement::Expression(
                        self.parse_component_values(/* allow_comma */ true)?,
                    ));
                    expect!(self, RBrace);
                }
                token => {
                    return Err(Error {
                        kind: ErrorKind::Unexpected("<string template> or '{'", token.symbol()),
                        span: token.span().clone(),
                    })
                }
            }
        }

        Ok(SassInterpolatedStr { elements, span })
    }

    pub(super) fn parse_sass_interpolated_url(&mut self) -> PResult<SassInterpolatedUrl<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let first = expect!(self, UrlTemplate);
        let mut span = first.span.clone();
        let mut elements = vec![SassInterpolatedUrlElement::Static(
            InterpolableUrlStaticPart {
                value: first.value,
                raw: first.raw,
                span: first.span,
            },
        )];

        loop {
            match self.tokenizer.bump()? {
                Token::UrlTemplate(token) => {
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
                }
                // '#' is consumed, so '{' left only
                Token::LBrace(..) => {
                    elements.push(SassInterpolatedUrlElement::Expression(
                        self.parse_component_values(/* allow_comma */ true)?,
                    ));
                    expect!(self, RBrace);
                }
                token => {
                    return Err(Error {
                        kind: ErrorKind::Unexpected("<url template> or '{'", token.symbol()),
                        span: token.span().clone(),
                    })
                }
            }
        }

        Ok(SassInterpolatedUrl { elements, span })
    }

    pub(super) fn parse_sass_parenthesized_expression(
        &mut self,
    ) -> PResult<SassParenthesizedExpression<'s>> {
        let l_paren = expect!(self, LParen);
        let expr = Box::new(self.parse_component_value()?);
        let r_paren = expect!(self, RParen);
        Ok(SassParenthesizedExpression {
            expr,
            span: Span {
                start: l_paren.span.start,
                end: r_paren.span.end,
            },
        })
    }

    pub(super) fn parse_sass_placeholder_selector(
        &mut self,
    ) -> PResult<SassPlaceholderSelector<'s>> {
        let percent = expect!(self, Percent);
        let name = self.parse_interpolable_ident()?;
        let span = Span {
            start: percent.span.start,
            end: name.span().end,
        };
        Ok(SassPlaceholderSelector { name, span })
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
            _ => return self.parse_component_value_internally(),
        };

        let expr = self.parse_component_value_internally()?;
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

    pub(super) fn parse_sass_variable(&mut self) -> PResult<SassVariable<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let dollar_var = expect!(self, DollarVar);
        Ok(SassVariable {
            name: dollar_var.ident.into(),
            span: dollar_var.span,
        })
    }

    pub(super) fn parse_sass_variable_declaration(
        &mut self,
    ) -> PResult<SassVariableDeclaration<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));

        let name = self.parse_sass_variable()?;
        expect!(self, Colon);
        let value = self.parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(SassVariableDeclaration { name, value, span })
    }

    pub(super) fn parse_sass_warn_at_rule(&mut self) -> PResult<SassWarnAtRule<'s>> {
        let token = expect!(self, AtKeyword);
        debug_assert_eq!(&*token.ident.name, "warn");
        let expr = self.parse_component_values(/* allow_comma */ true)?;
        let span = Span {
            start: token.span.start,
            end: expr.span.end,
        };
        Ok(SassWarnAtRule { expr, span })
    }

    pub(super) fn parse_sass_while_at_rule(&mut self) -> PResult<SassWhileAtRule<'s>> {
        let at_keyword = expect!(self, AtKeyword);
        debug_assert_eq!(&*at_keyword.ident.name, "while");
        let condition = self.parse_component_value()?;
        let body = self.parse_simple_block()?;
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
