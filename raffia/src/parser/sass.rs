use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_sass_expression(
        &mut self,
        allow_comma: bool,
    ) -> PResult<SassExpression<'a>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss));

        let first = self.parse_sass_expression_child()?;
        let mut span = first.span().clone();

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match self.tokenizer.peek()? {
                Token::RBrace(..) | Token::RParen(..) | Token::Semicolon(..) | Token::Eof => break,
                Token::Comma(..) => {
                    if allow_comma {
                        elements.push(self.parse_sass_expression_child()?);
                    } else {
                        break;
                    }
                }
                _ => elements.push(self.parse_sass_expression_child()?),
            }
        }

        if let Some(last) = elements.last() {
            span.end = last.span().end;
        }
        Ok(SassExpression { elements, span })
    }

    fn parse_sass_expression_child(&mut self) -> PResult<SassExpressionChild<'a>> {
        match self.tokenizer.peek()? {
            Token::Ident(..) | Token::HashLBrace(..) => self
                .parse_sass_interpolated_ident()
                .map(SassExpressionChild::InterpolableIdent),
            Token::Function(..) => self.parse_function().map(SassExpressionChild::Function),
            Token::Number(..) => self.parse_number().map(SassExpressionChild::Number),
            Token::Dimension(..) => self.parse_dimension().map(SassExpressionChild::Dimension),
            Token::Percentage(..) => self.parse_percentage().map(SassExpressionChild::Percentage),
            Token::Hash(..) => self.parse_hex_color().map(SassExpressionChild::HexColor),
            Token::Str(..) => self.parse_str().map(SassExpressionChild::Str),
            Token::DollarVar(..) => self
                .parse_sass_variable()
                .map(SassExpressionChild::SassVariable),
            token => Err(Error {
                kind: ErrorKind::ExpectSassExpression,
                span: token.span().clone(),
            }),
        }
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
        let expr = self.parse_sass_expression(/* allow_comma */ true)?;
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
        let value = self.parse_sass_expression(/* allow_comma */ true)?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(SassVariableDeclaration { name, value, span })
    }
}
