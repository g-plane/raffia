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
    pub(super) fn parse_less_property_merge(&mut self) -> PResult<Option<LessPropertyMerge>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        match self.tokenizer.peek()? {
            Token::Plus(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(LessPropertyMerge {
                    kind: LessPropertyMergeKind::Comma,
                    span: token.span,
                }))
            }
            Token::PlusUnderscore(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(LessPropertyMerge {
                    kind: LessPropertyMergeKind::Space,
                    span: token.span,
                }))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn parse_less_variable(&mut self) -> PResult<LessVariable<'a>> {
        let at_keyword = expect!(self, AtKeyword);
        Ok(LessVariable {
            name: at_keyword.ident.into(),
            span: at_keyword.span,
        })
    }

    pub(super) fn parse_less_variable_declaration(
        &mut self,
    ) -> PResult<LessVariableDeclaration<'a>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let name = self.parse_less_variable()?;
        expect!(self, Colon);
        let value = self.parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(LessVariableDeclaration { name, value, span })
    }
}
