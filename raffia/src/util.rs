use crate::{
    error::{Error, ErrorKind, PResult},
    Span,
};
use std::borrow::Cow;

pub type CowStr<'s> = Cow<'s, str>;

pub fn is_css_wide_keyword(s: &str) -> bool {
    s.eq_ignore_ascii_case("initial")
        || s.eq_ignore_ascii_case("inherit")
        || s.eq_ignore_ascii_case("unset")
        || s.eq_ignore_ascii_case("revert")
        || s.eq_ignore_ascii_case("revert-layer")
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

pub(crate) fn assert_no_ws_or_comment(left: &Span, right: &Span) -> PResult<()> {
    debug_assert!(left.end <= right.start);
    if left.end == right.start {
        Ok(())
    } else {
        Err(Error {
            kind: ErrorKind::UnexpectedWhitespace,
            span: Span {
                start: left.end,
                end: right.start,
            },
        })
    }
}
