use super::{TokenWithSpan, token::Ident};
use crate::{SpanIgnoredEq, util};
use std::borrow::Cow;

impl<'s> Ident<'s> {
    #[inline]
    pub fn name(&self) -> Cow<'s, str> {
        if self.escaped {
            util::handle_escape(self.raw)
        } else {
            Cow::from(self.raw)
        }
    }
}

impl SpanIgnoredEq for TokenWithSpan<'_> {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self.token == other.token
    }
}
