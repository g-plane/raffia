use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
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

impl<'a> Parser<'a> {
    pub(super) fn parse_sass_bin_expr(&mut self) -> PResult<ComponentValue<'a>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss));
        self.parse_sass_bin_expr_recursively(0)
    }

    fn parse_sass_bin_expr_recursively(&mut self, precedence: u8) -> PResult<ComponentValue<'a>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            self.parse_component_value_internally()?
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

    pub(super) fn parse_sass_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'a>> {
        let (first, mut span) = match self.tokenizer.peek()? {
            Token::Ident(ident) => {
                self.tokenizer.bump()?;
                let span = ident.span.clone();
                match self.tokenizer.peek()? {
                    Token::HashLBrace(token) if ident.span.end == token.span.start => {
                        (SassInterpolatedIdentElement::Literal(ident.into()), span)
                    }
                    _ => return Ok(InterpolableIdent::Literal(ident.into())),
                }
            }
            Token::HashLBrace(..) => self.parse_sass_interpolated_ident_expr()?,
            _ => unreachable!(),
        };
        let mut last_span_end = span.end;

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match self.tokenizer.peek()? {
                Token::Ident(token) if last_span_end == token.span.start => {
                    last_span_end = token.span.end;
                    self.tokenizer.bump()?;
                    elements.push(SassInterpolatedIdentElement::Literal(token.into()));
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
    ) -> PResult<(SassInterpolatedIdentElement<'a>, Span)> {
        debug_assert!(matches!(self.syntax, Syntax::Scss));

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

    pub(super) fn parse_sass_variable(&mut self) -> PResult<SassVariable<'a>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss));

        let dollar_var = expect!(self, DollarVar);
        Ok(SassVariable {
            name: dollar_var.ident.into(),
            span: dollar_var.span,
        })
    }

    pub(super) fn parse_sass_variable_declaration(
        &mut self,
    ) -> PResult<SassVariableDeclaration<'a>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss));

        let name = self.parse_sass_variable()?;
        expect!(self, Colon);
        let value = self.parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(SassVariableDeclaration { name, value, span })
    }
}
