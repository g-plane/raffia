use smallvec::SmallVec;
use std::borrow::Cow;

pub type CowStr<'s> = Cow<'s, str>;

pub fn is_css_wide_keyword(s: &str) -> bool {
    s.eq_ignore_ascii_case("initial")
        || s.eq_ignore_ascii_case("inherit")
        || s.eq_ignore_ascii_case("unset")
}

pub trait LastOfNonEmpty<T> {
    /// Return the last element of the given vector.
    ///
    /// Make sure the given vector is non-empty, otherwise it would lead to Undefined Behavior.
    fn last_of_non_empty(&self) -> &T;
}

impl<T, const N: usize> LastOfNonEmpty<T> for SmallVec<[T; N]> {
    #[inline]
    fn last_of_non_empty(&self) -> &T {
        let len = self.len();
        unsafe { self.get_unchecked(len - 1) }
    }
}

impl<T> LastOfNonEmpty<T> for Vec<T> {
    #[inline]
    fn last_of_non_empty(&self) -> &T {
        let len = self.len();
        unsafe { self.get_unchecked(len - 1) }
    }
}

/// `PairedToken` is used for tracking when parsing with raw tokens.
pub(crate) enum PairedToken {
    Paren,
    Bracket,
    Brace,
}
