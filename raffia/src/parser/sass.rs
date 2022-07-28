use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
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
            Token::Ident(..) => self.parse_ident().map(SassExpressionChild::Ident),
            Token::Function(..) => self.parse_function().map(SassExpressionChild::Function),
            Token::Number(..) => self.parse_number().map(SassExpressionChild::Number),
            Token::Dimension(..) => self.parse_dimension().map(SassExpressionChild::Dimension),
            Token::Percentage(..) => self.parse_percentage().map(SassExpressionChild::Percentage),
            Token::Hash(..) => self.parse_hex_color().map(SassExpressionChild::HexColor),
            Token::Str(..) => self.parse_str().map(SassExpressionChild::Str),
            Token::DollarVar(..) => self
                .parse_sass_variable()
                .map(SassExpressionChild::SassVariable),
            _ => Err(panic!()),
        }
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
