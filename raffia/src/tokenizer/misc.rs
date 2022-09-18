use super::{token::Ident, TokenWithSpan};
use crate::{
    util::{handle_escape, CowStr},
    SpanIgnoredEq,
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
}

impl SpanIgnoredEq for TokenWithSpan<'_> {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self.token == other.token
    }
}
