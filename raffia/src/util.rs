use smallvec::SmallVec;
use std::borrow::Cow;

pub type CowStr<'s> = Cow<'s, str>;

pub fn is_css_wide_keyword(s: &str) -> bool {
    s.eq_ignore_ascii_case("initial")
        || s.eq_ignore_ascii_case("inherit")
        || s.eq_ignore_ascii_case("unset")
        || s.eq_ignore_ascii_case("revert")
        || s.eq_ignore_ascii_case("revert-layer")
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

pub fn handle_escape(s: &str) -> CowStr {
    let mut escaped = String::with_capacity(s.len());
    let mut chars = s.char_indices().peekable();
    while let Some((_, c)) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some((start, c)) if c.is_ascii_hexdigit() => {
                    let mut count: usize = 1;
                    while let Some((_, c)) = chars.peek() {
                        if c.is_ascii_hexdigit() && count < 6 {
                            count += 1;
                            chars.next();
                        } else {
                            // according to https://www.w3.org/TR/css-syntax-3/#hex-digit,
                            // consume a whitespace
                            if c.is_ascii_whitespace() {
                                chars.next();
                            }
                            break;
                        }
                    }
                    let unicode = s
                        .get(start..start + count)
                        .and_then(|hexdigits| u32::from_str_radix(hexdigits, 16).ok())
                        .expect("expect unicode value"); // this line should be unreachable
                    escaped.push(char::from_u32(unicode).unwrap_or(char::REPLACEMENT_CHARACTER));
                }
                Some((_, c)) => escaped.push(c),
                None => unreachable!(),
            }
        } else {
            escaped.push(c);
        }
    }
    CowStr::from(escaped)
}
