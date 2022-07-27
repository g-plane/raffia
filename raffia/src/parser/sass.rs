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
}
