use crate::{
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    pos::Span,
};
use std::{borrow::Cow, iter::Peekable, str::CharIndices};
pub use token::Token;
use token::*;

mod convert;
pub mod token;

/// Newtype for wrapping tokenizer state to avoid exposing internal detail.
#[derive(Clone)]
pub(crate) struct TokenizerState<'a>(Peekable<CharIndices<'a>>);

pub struct Tokenizer<'a> {
    source: &'a str,
    syntax: Syntax,
    iter: Peekable<CharIndices<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(source: &'a str, syntax: Syntax) -> Self {
        Self {
            source,
            syntax,
            iter: source.char_indices().peekable(),
        }
    }

    pub fn bump(&mut self) -> PResult<Token<'a>> {
        self.skip_ws_or_comment();

        match self.peek_two_chars() {
            Some((_, '\'' | '"', _)) => self.scan_string().map(Token::Str),
            Some((_, '-' | '+', c)) if c.is_ascii_digit() || c == '.' => {
                let number = self.scan_number()?;
                self.scan_dimension_or_percentage(number)
            }
            Some((_, '-', c))
                if c.is_ascii_alphabetic()
                    || c == '-'
                    || c == '_'
                    || !c.is_ascii()
                    || c == '\\' =>
            {
                self.scan_ident_or_function_or_url()
            }
            Some((_, '.', c)) if c.is_ascii_digit() => {
                let number = self.scan_number()?;
                self.scan_dimension_or_percentage(number)
            }
            Some((_, '#', c))
                if c.is_ascii_alphanumeric()
                    || c == '-'
                    || c == '_'
                    || !c.is_ascii()
                    || c == '\\' =>
            {
                self.scan_hash()
            }
            Some((_, '@', c))
                if c.is_ascii_alphabetic()
                    || c == '-'
                    || c == '_'
                    || !c.is_ascii()
                    || c == '\\' =>
            {
                self.scan_at_keyword()
            }
            Some((_, '$', c))
                if c.is_ascii_alphabetic()
                    || c == '-'
                    || c == '_'
                    || !c.is_ascii()
                    || c == '\\' =>
            {
                self.scan_dollar_var()
            }
            _ => match self.peek_one_char() {
                Some((_, c)) if c.is_ascii_digit() => {
                    let number = self.scan_number()?;
                    self.scan_dimension_or_percentage(number)
                }
                Some((_, c))
                    if c.is_ascii_alphabetic() || c == '_' || !c.is_ascii() || c == '\\' =>
                {
                    self.scan_ident_or_function_or_url()
                }
                Some((i, c)) => self.scan_punc().ok_or_else(|| Error {
                    kind: ErrorKind::UnknownToken,
                    span: Span {
                        start: i,
                        end: i + c.len_utf8(),
                    },
                }),
                None => Ok(Token::Eof),
            },
        }
    }

    pub fn peek(&mut self) -> PResult<Token<'a>> {
        let iter = self.iter.clone();
        let token = self.bump();
        self.iter = iter;
        token
    }

    pub fn current_offset(&self) -> usize {
        self.iter
            .clone()
            .next()
            .map(|(i, _)| i)
            .unwrap_or_else(|| self.source.len())
    }

    pub(crate) fn clone_state(&self) -> TokenizerState<'a> {
        TokenizerState(self.iter.clone())
    }

    pub(crate) fn replace_state(&mut self, state: TokenizerState<'a>) {
        self.iter = state.0;
    }

    fn peek_one_char(&self) -> Option<(usize, char)> {
        self.iter.clone().next()
    }

    fn peek_two_chars(&self) -> Option<(usize, char, char)> {
        let mut iter = self.iter.clone();
        iter.next()
            .zip(iter.next())
            .map(|((start, first), (_, second))| (start, first, second))
    }

    fn skip_ws_or_comment(&mut self) {
        loop {
            match self.peek_two_chars() {
                Some((_, '/', '*')) => self.scan_block_comment(),
                Some((_, '/', '/')) if self.syntax != Syntax::Css => self.scan_line_comment(),
                Some((_, c, _)) if c.is_ascii_whitespace() => self.skip_ws(),
                _ => return,
            }
        }
    }

    fn skip_ws(&mut self) {
        while let Some((_, c)) = self.iter.peek() {
            if c.is_ascii_whitespace() {
                self.iter.next();
            } else {
                break;
            }
        }
    }

    fn scan_block_comment(&mut self) {
        let start = if let Some((i, '/')) = self.iter.next() {
            i
        } else {
            return;
        };
        let content_start = if let Some((i, '*')) = self.iter.next() {
            i + 1
        } else {
            return;
        };

        let content_end;
        let end;
        loop {
            match self.peek_two_chars() {
                Some((i, '*', '/')) => {
                    content_end = i;
                    end = i + 2;

                    self.iter.next();
                    self.iter.next();
                    break;
                }
                Some(..) => {
                    self.iter.next();
                }
                None => {
                    content_end = self.source.len();
                    end = content_end;
                    break;
                }
            }
        }

        let content = unsafe { self.source.get_unchecked(content_start..content_end) };
        BlockComment {
            content,
            span: Span { start, end },
        };
    }

    fn scan_line_comment(&mut self) {
        let start = if let Some((i, '/')) = self.iter.next() {
            i
        } else {
            return;
        };
        let content_start = if let Some((i, '/')) = self.iter.next() {
            i + 1
        } else {
            return;
        };

        let content_end;
        let end;
        loop {
            match self.peek_two_chars() {
                Some((i, '\r', '\n')) => {
                    content_end = i;
                    end = i;
                    self.iter.next();
                    self.iter.next();
                    break;
                }
                Some((i, '\n', _)) => {
                    content_end = i;
                    end = i;
                    self.iter.next();
                    break;
                }
                Some(..) => {
                    self.iter.next();
                }
                None => {
                    content_end = if let Some((i, '\n')) = self.peek_one_char() {
                        self.iter.next();
                        i
                    } else {
                        self.source.len()
                    };
                    end = content_end;
                    break;
                }
            }
        }

        let content = unsafe { self.source.get_unchecked(content_start..content_end) };
        LineComment {
            content,
            span: Span { start, end },
        };
    }

    fn scan_ident_sequence(&mut self) -> PResult<Ident<'a>> {
        let start;
        let mut end;
        match self.peek_one_char() {
            Some((i, '-')) => {
                start = i;
                if let Some((i, c)) = self.iter.next() {
                    debug_assert!(
                        c.is_ascii_alphabetic()
                            || c == '-'
                            || c == '_'
                            || !c.is_ascii()
                            || c == '\\'
                    );
                    self.iter.next();
                    end = i + c.len_utf8();
                } else {
                    return Err(Error {
                        kind: ErrorKind::UnexpectedEof,
                        span: Span {
                            start: self.current_offset(),
                            end: self.current_offset(),
                        },
                    });
                }
            }
            Some((i, c)) if c.is_ascii_alphabetic() || c == '-' || !c.is_ascii() || c == '\\' => {
                start = i;
                self.iter.next();
                end = i + c.len_utf8()
            }
            _ => {
                return Err(Error {
                    kind: ErrorKind::UnexpectedEof,
                    span: Span {
                        start: self.current_offset(),
                        end: self.current_offset(),
                    },
                });
            }
        }

        while let Some((i, c)) = self.peek_one_char() {
            if c.is_ascii_alphanumeric() || c == '-' || c == '_' || !c.is_ascii() || c == '\\' {
                self.iter.next();
            } else {
                end = i;
                break;
            }
        }

        assert!(start < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let span = Span { start, end };
        Ok(Ident {
            name: handle_escape(raw).map_err(|kind| Error {
                kind,
                span: span.clone(),
            })?,
            raw,
            span,
        })
    }

    fn scan_number(&mut self) -> PResult<Number<'a>> {
        let start;
        let mut end = 0;

        let is_start_with_dot;
        if let Some((i, c)) = self.iter.next() {
            start = i;
            if c.is_ascii_digit() {
                is_start_with_dot = false;
                end = i + 1;
            } else if c == '+' || c == '-' {
                is_start_with_dot = if let Some((_, '.')) = self.iter.peek() {
                    self.iter.next();
                    true
                } else {
                    false
                };
            } else if c == '.' {
                is_start_with_dot = true;
            } else {
                return Err(Error {
                    kind: ErrorKind::InvalidNumber,
                    span: Span {
                        start: i,
                        end: i + c.len_utf8(),
                    },
                });
            }
        } else {
            return Err(Error {
                kind: ErrorKind::UnexpectedEof,
                span: Span {
                    start: self.current_offset(),
                    end: self.current_offset(),
                },
            });
        }

        if is_start_with_dot {
            while let Some((i, c)) = self.peek_one_char() {
                if c.is_ascii_digit() {
                    self.iter.next();
                } else {
                    end = i;
                    break;
                }
            }
        } else {
            while let Some((i, c)) = self.peek_one_char() {
                if c.is_ascii_digit() {
                    self.iter.next();
                } else {
                    end = i;
                    break;
                }
            }
            if let Some((_, '.')) = self.iter.peek() {
                // bump '.'
                self.iter.next();
                while let Some((i, c)) = self.peek_one_char() {
                    if c.is_ascii_digit() {
                        self.iter.next();
                    } else {
                        end = i;
                        break;
                    }
                }
            }
        }

        match self.peek_two_chars() {
            Some((_, 'e' | 'E', second))
                if second == '-' || second == '+' || second.is_ascii_digit() =>
            {
                self.iter.next();

                if let Some((_, '-' | '+')) = self.iter.peek() {
                    self.iter.next();
                }

                while let Some((i, c)) = self.iter.clone().peek() {
                    if c.is_ascii_digit() {
                        self.iter.next();
                    } else {
                        end = *i;
                        break;
                    }
                }
            }
            _ => {}
        }

        assert!(start < end);
        let span = Span { start, end };
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok(Number {
            value: raw.parse().map_err(|_| Error {
                kind: ErrorKind::InvalidNumber,
                span: span.clone(),
            })?,
            raw,
            span,
        })
    }

    fn scan_dimension_or_percentage(&mut self, number: Number<'a>) -> PResult<Token<'a>> {
        match self.iter.peek() {
            Some((_, c))
                if c.is_ascii_alphabetic()
                    || *c == '-'
                    || *c == '_'
                    || *c == '\\'
                    || !c.is_ascii() =>
            {
                self.scan_dimension(number)
            }
            Some((_, '%')) => self.scan_percentage(number),
            _ => Ok(Token::Number(number)),
        }
    }

    fn scan_dimension(&mut self, value: Number<'a>) -> PResult<Token<'a>> {
        let unit = self.scan_ident_sequence()?;
        let span = Span {
            start: value.span.start,
            end: unit.span.end,
        };
        Ok(Token::Dimension(Dimension { value, unit, span }))
    }

    fn scan_percentage(&mut self, value: Number<'a>) -> PResult<Token<'a>> {
        let start = value.span.start;
        let (i, c) = self.iter.next().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: self.current_offset(),
                end: self.current_offset(),
            },
        })?;
        debug_assert_eq!(c, '%');
        Ok(Token::Percentage(Percentage {
            value,
            span: Span { start, end: i + 1 },
        }))
    }

    fn scan_string(&mut self) -> PResult<Str<'a>> {
        // '\'' or '"' is checked (but not consumed) before
        let (start, quote) = self.iter.next().unwrap();

        let mut end = 0;
        for (i, c) in self.iter.by_ref() {
            if c == quote {
                end = i + c.len_utf8();
                break;
            }
        }

        assert!(start + 1 < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let value = unsafe { self.source.get_unchecked(start + 1..end - 1) };
        let span = Span { start, end };
        Ok(Str {
            raw,
            value: handle_escape(value).map_err(|kind| Error {
                kind,
                span: span.clone(),
            })?,
            span,
        })
    }

    fn scan_ident_or_function_or_url(&mut self) -> PResult<Token<'a>> {
        let ident = self.scan_ident_sequence()?;
        if let Some((_, '(')) = self.iter.peek() {
            if ident.name.eq_ignore_ascii_case("url") {
                self.scan_url(ident.span.start).map(Token::Url)
            } else {
                self.scan_function(ident).map(Token::Function)
            }
        } else {
            Ok(Token::Ident(ident))
        }
    }

    fn scan_function(&mut self, name: Ident<'a>) -> PResult<Function<'a>> {
        let (i, c) = self.iter.next().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: self.current_offset(),
                end: self.current_offset(),
            },
        })?;
        debug_assert_eq!(c, '(');
        let start = name.span.start;
        let end = i + 1;
        Ok(Function {
            name,
            span: Span { start, end },
        })
    }

    fn scan_url(&mut self, start: usize) -> PResult<Url<'a>> {
        let (_, c) = self.iter.next().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: self.current_offset(),
                end: self.current_offset(),
            },
        })?;
        debug_assert_eq!(c, '(');

        self.skip_ws();
        if let Some((_, '\'' | '"')) = self.iter.peek() {
            let value = self.scan_string().map(UrlValue::Str)?;
            match self.iter.next() {
                Some((i, ')')) => Ok(Url {
                    value,
                    span: Span { start, end: i + 1 },
                }),
                Some((i, c)) => Err(Error {
                    kind: ErrorKind::ExpectRightParenForURL,
                    span: Span {
                        start,
                        end: i + c.len_utf8(),
                    },
                }),
                None => Err(Error {
                    kind: ErrorKind::UnexpectedEof,
                    span: Span {
                        start: self.current_offset(),
                        end: self.current_offset(),
                    },
                }),
            }
        } else {
            // ')' is consumed
            let value = self.scan_url_raw().map(UrlValue::Raw)?;
            Ok(Url {
                value,
                span: Span {
                    start,
                    end: self.current_offset(),
                },
            })
        }
    }

    fn scan_url_raw(&mut self) -> PResult<UrlRaw<'a>> {
        let start = self.current_offset();
        let mut end = self.source.len();
        for (i, c) in self.iter.by_ref() {
            if c == ')' {
                end = i;
                break;
            }
        }
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let span = Span { start, end };
        Ok(UrlRaw {
            value: handle_escape(raw).map_err(|kind| Error {
                kind,
                span: span.clone(),
            })?,
            raw,
            span,
        })
    }

    fn scan_hash(&mut self) -> PResult<Token<'a>> {
        let (start, c) = self.iter.next().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: self.current_offset(),
                end: self.current_offset(),
            },
        })?;
        debug_assert_eq!(c, '#');

        let mut end;
        match self.iter.next() {
            Some((i, c))
                if c.is_ascii_alphanumeric()
                    || c == '-'
                    || c == '_'
                    || !c.is_ascii()
                    || c == '\\' =>
            {
                end = i + c.len_utf8();
            }
            Some((i, _)) => {
                return Err(Error {
                    kind: ErrorKind::InvalidHash,
                    span: Span {
                        start: i,
                        end: i + c.len_utf8(),
                    },
                })
            }
            None => {
                return Err(Error {
                    kind: ErrorKind::UnexpectedEof,
                    span: Span {
                        start: self.current_offset(),
                        end: self.current_offset(),
                    },
                })
            }
        }
        while let Some((i, c)) = self.peek_one_char() {
            if c.is_ascii_alphanumeric() || c == '-' || c == '_' || !c.is_ascii() || c == '\\' {
                self.iter.next();
            } else {
                end = i;
                break;
            }
        }

        assert!(end > start + 1);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let value = unsafe { self.source.get_unchecked(start + 1..end) };
        let span = Span { start, end };
        Ok(Token::Hash(Hash {
            value: handle_escape(value).map_err(|kind| Error {
                kind,
                span: span.clone(),
            })?,
            raw,
            raw_without_hash: value,
            span,
        }))
    }

    fn scan_dollar_var(&mut self) -> PResult<Token<'a>> {
        let (start, c) = self.iter.next().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: self.current_offset(),
                end: self.current_offset(),
            },
        })?;
        debug_assert_eq!(c, '$');
        let ident = self.scan_ident_sequence()?;
        let span = Span {
            start,
            end: ident.span.end,
        };
        Ok(Token::DollarVar(DollarVar { ident, span }))
    }

    fn scan_at_keyword(&mut self) -> PResult<Token<'a>> {
        let (start, c) = self.iter.next().ok_or_else(|| Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: self.current_offset(),
                end: self.current_offset(),
            },
        })?;
        debug_assert_eq!(c, '@');
        let ident = self.scan_ident_sequence()?;
        let span = Span {
            start,
            end: ident.span.end,
        };
        Ok(Token::AtKeyword(AtKeyword { ident, span }))
    }

    fn scan_punc(&mut self) -> Option<Token<'a>> {
        match self.peek_two_chars() {
            Some((i, ':', ':')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::ColonColon(ColonColon {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '|', '|')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::BarBar(BarBar {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '~', '=')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::TildeEqual(TildeEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '|', '=')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::BarEqual(BarEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '^', '=')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::CaretEqual(CaretEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '$', '=')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::DollarEqual(DollarEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '*', '=')) => {
                self.iter.next();
                self.iter.next();
                Some(Token::AsteriskEqual(AsteriskEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '#', '{')) if matches!(self.syntax, Syntax::Scss) => {
                self.iter.next();
                self.iter.next();
                Some(Token::HashLBrace(HashLBrace {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            _ => match self.iter.next() {
                Some((i, ':')) => Some(Token::Colon(Colon {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '(')) => Some(Token::LParen(LParen {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, ')')) => Some(Token::RParen(RParen {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '[')) => Some(Token::LBracket(LBracket {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, ']')) => Some(Token::RBracket(RBracket {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '{')) => Some(Token::LBrace(LBrace {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '}')) => Some(Token::RBrace(RBrace {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '/')) => Some(Token::Solidus(Solidus {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, ',')) => Some(Token::Comma(Comma {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, ';')) => Some(Token::Semicolon(Semicolon {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '.')) => Some(Token::Dot(Dot {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '>')) => Some(Token::GreaterThan(GreaterThan {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '+')) => Some(Token::Plus(Plus {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '~')) => Some(Token::Tilde(Tilde {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '&')) => Some(Token::Ampersand(Ampersand {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '*')) => Some(Token::Asterisk(Asterisk {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '|')) => Some(Token::Bar(Bar {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '=')) => Some(Token::Equal(Equal {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                _ => None,
            },
        }
    }
}

fn handle_escape(s: &str) -> Result<Cow<str>, ErrorKind> {
    if s.contains('\\') {
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
                            .ok_or(ErrorKind::InvalidEscape)?;
                        escaped
                            .push(char::from_u32(unicode).unwrap_or(char::REPLACEMENT_CHARACTER));
                    }
                    Some((_, c)) => escaped.push(c),
                    None => return Err(ErrorKind::InvalidEscape),
                }
            } else {
                escaped.push(c);
            }
        }
        Ok(escaped.into())
    } else {
        Ok(s.into())
    }
}
