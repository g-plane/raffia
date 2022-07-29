use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    expect,
    pos::{Span, Spanned},
};

impl<'a> Parser<'a> {
    pub(super) fn parse_less_variable_declaration(
        &mut self,
    ) -> PResult<LessVariableDeclaration<'a>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let at_keyword = expect!(self, AtKeyword);
        expect!(self, Colon);
        let value = self.parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: at_keyword.span.start,
            end: value.span.end,
        };
        Ok(LessVariableDeclaration {
            name: at_keyword.ident.into(),
            value,
            span,
        })
    }
}
