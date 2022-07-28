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

        let first = self.parse_sass_expression_element()?;
        let mut span = first.span().clone();

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match self.tokenizer.peek() {
                Token::RBrace(..) | Token::RParen(..) | Token::Semicolon(..) | Token::Eof => break,
                Token::Comma(..) => {
                    if allow_comma {
                        elements.push(self.parse_sass_expression_element()?);
                    } else {
                        break;
                    }
                }
                _ => match self.parse_sass_expression_element() {
                    Ok(value) => elements.push(value),
                    Err(error) => return Err(error),
                },
            }
        }

        if let Some(last) = elements.last() {
            span.end = last.span().end;
        }
        Ok(SassExpression { elements, span })
    }

    fn parse_sass_expression_element(&mut self) -> PResult<SassExpressionElement<'a>> {
        match self.tokenizer.peek() {
            Token::Ident(..) => self.parse_ident().map(SassExpressionElement::Ident),
            Token::Function(..) => self.parse_function().map(SassExpressionElement::Function),
            Token::Number(..) => self.parse_number().map(SassExpressionElement::Number),
            Token::Dimension(..) => self.parse_dimension().map(SassExpressionElement::Dimension),
            Token::Percentage(..) => self
                .parse_percentage()
                .map(SassExpressionElement::Percentage),
            Token::Hash(..) => self.parse_hex_color().map(SassExpressionElement::HexColor),
            Token::Str(..) => self.parse_str().map(SassExpressionElement::Str),
            Token::DollarVar(..) => self
                .parse_sass_variable()
                .map(SassExpressionElement::SassVariable),
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
