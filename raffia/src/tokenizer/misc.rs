use super::{token::Ident, TokenWithSpan};
use crate::{
    ast,
    util::{handle_escape, CowStr},
    Span, SpanIgnoredEq,
};

impl<'s> Ident<'s> {
    #[inline]
    pub fn name(&self) -> CowStr<'s> {
        if self.escaped {
            handle_escape(self.raw)
        } else {
            CowStr::from(self.raw)
        }
    }

    pub(crate) fn into_with_span(self, span: Span) -> ast::Ident<'s> {
        ast::Ident {
            name: self.name(),
            raw: self.raw,
            span,
        }
    }
}

impl SpanIgnoredEq for TokenWithSpan<'_> {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self.token == other.token
    }
}
