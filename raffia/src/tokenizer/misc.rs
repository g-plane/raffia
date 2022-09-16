use super::token::Ident;
use crate::util::{handle_escape, CowStr};

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
