use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    expect,
    pos::{Span, Spanned},
};

impl<'a> Parser<'a> {
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
        let value = self.parse_declaration_value()?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(SassVariableDeclaration { name, value, span })
    }
}
