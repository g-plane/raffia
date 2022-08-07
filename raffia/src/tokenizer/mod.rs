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
pub(crate) struct TokenizerState<'s> {
    chars: Peekable<CharIndices<'s>>,
    indent_size: usize,
}

pub struct Tokenizer<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    pub(crate) comments: Option<&'cmt mut Vec<Comment<'s>>>,
    pub(crate) state: TokenizerState<'s>,
}

impl<'cmt, 's: 'cmt> Tokenizer<'cmt, 's> {
    pub fn new(
        source: &'s str,
        syntax: Syntax,
        comments: Option<&'cmt mut Vec<Comment<'s>>>,
    ) -> Self {
        Self {
            source,
            syntax,
            comments,
            state: TokenizerState {
                chars: source.char_indices().peekable(),
                indent_size: 0,
            },
        }
    }

    pub fn bump(&mut self) -> PResult<Token<'s>> {
        if let Some(indent) = self.skip_ws_or_comment() {
            return Ok(indent);
        }

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
                self.scan_ident_or_url()
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
                if matches!(self.syntax, Syntax::Scss | Syntax::Sass)
                    && (c.is_ascii_alphabetic()
                        || c == '-'
                        || c == '_'
                        || !c.is_ascii()
                        || c == '\\') =>
            {
                self.scan_dollar_var()
            }
            Some((_, '@', '{')) if self.syntax == Syntax::Less => self.scan_at_lbrace_var(),
            _ => match self.peek_one_char() {
                Some((_, c)) if c.is_ascii_digit() => {
                    let number = self.scan_number()?;
                    self.scan_dimension_or_percentage(number)
                }
                Some((_, c))
                    if c.is_ascii_alphabetic() || c == '_' || !c.is_ascii() || c == '\\' =>
                {
                    self.scan_ident_or_url()
                }
                Some((i, c)) => self.scan_punc().ok_or_else(|| Error {
                    kind: ErrorKind::UnknownToken,
                    span: Span {
                        start: i,
                        end: i + c.len_utf8(),
                    },
                }),
                None => {
                    let offset = self.current_offset();
                    Ok(Token::Eof(Eof {
                        span: Span {
                            start: offset,
                            end: offset,
                        },
                    }))
                }
            },
        }
    }

    pub fn peek(&mut self) -> PResult<Token<'s>> {
        let state = self.state.clone();
        let comments = self.comments.take();

        let token = self.bump();
        self.state = state;
        self.comments = comments;
        token
    }

    pub fn current_offset(&self) -> usize {
        self.state
            .chars
            .clone()
            .next()
            .map(|(i, _)| i)
            .unwrap_or_else(|| self.source.len())
    }

    fn peek_one_char(&self) -> Option<(usize, char)> {
        self.state.chars.clone().next()
    }

    fn peek_two_chars(&self) -> Option<(usize, char, char)> {
        let mut iter = self.state.chars.clone();
        iter.next()
            .zip(iter.next())
            .map(|((start, first), (_, second))| (start, first, second))
    }

    fn build_eof_error(&self) -> Error {
        let offset = self.current_offset();
        Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: offset,
                end: offset,
            },
        }
    }

    fn skip_ws_or_comment(&mut self) -> Option<Token<'s>> {
        let mut indent = None;
        loop {
            match self.peek_two_chars() {
                Some((_, '/', '*')) => self.scan_block_comment(),
                Some((_, '/', '/')) if self.syntax != Syntax::Css => self.scan_line_comment(),
                _ => match self.state.chars.peek() {
                    Some((_, c)) if c.is_ascii_whitespace() => {
                        if self.syntax == Syntax::Sass {
                            indent = self.scan_indent();
                        } else {
                            self.skip_ws();
                        }
                    }
                    _ => return indent,
                },
            }
        }
    }

    fn skip_ws(&mut self) {
        while let Some((_, c)) = self.state.chars.peek() {
            if c.is_ascii_whitespace() {
                self.state.chars.next();
            } else {
                break;
            }
        }
    }

    fn scan_indent(&mut self) -> Option<Token<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Sass);
        let mut start = None;
        while let Some((i, c)) = self.state.chars.peek() {
            if c.is_ascii_whitespace() {
                let (i, c) = self.state.chars.next()?;
                if c == '\n' || c == '\r' && matches!(self.state.chars.peek(), Some((_, '\n'))) {
                    start = Some(i + 1);
                }
            } else {
                return start.map(|start| {
                    let end = *i;
                    let len = end - start;
                    let span = Span { start, end };
                    if len > self.state.indent_size {
                        self.state.indent_size = len;
                        Token::Indent(Indent { span })
                    } else if len < self.state.indent_size {
                        self.state.indent_size = len;
                        Token::Dedent(Dedent { span })
                    } else {
                        Token::Linebreak(Linebreak { span })
                    }
                });
            }
        }

        let offset = self.current_offset();
        Some(Token::Eof(Eof {
            span: Span {
                start: offset,
                end: offset,
            },
        }))
    }

    fn scan_block_comment(&mut self) {
        let start = if let Some((i, '/')) = self.state.chars.next() {
            i
        } else {
            return;
        };
        let content_start = if let Some((i, '*')) = self.state.chars.next() {
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

                    self.state.chars.next();
                    self.state.chars.next();
                    break;
                }
                Some(..) => {
                    self.state.chars.next();
                }
                None => {
                    content_end = self.source.len();
                    end = content_end;
                    break;
                }
            }
        }

        if let Some(comments) = &mut self.comments {
            let content = unsafe { self.source.get_unchecked(content_start..content_end) };
            comments.push(Comment::Block(BlockComment {
                content,
                span: Span { start, end },
            }));
        }
    }

    fn scan_line_comment(&mut self) {
        let start = if let Some((i, '/')) = self.state.chars.next() {
            i
        } else {
            return;
        };
        let content_start = if let Some((i, '/')) = self.state.chars.next() {
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
                    self.state.chars.next();
                    self.state.chars.next();
                    break;
                }
                Some((i, '\n', _)) => {
                    content_end = i;
                    end = i;
                    self.state.chars.next();
                    break;
                }
                Some(..) => {
                    self.state.chars.next();
                }
                None => {
                    content_end = if let Some((i, '\n')) = self.peek_one_char() {
                        self.state.chars.next();
                        i
                    } else {
                        self.source.len()
                    };
                    end = content_end;
                    break;
                }
            }
        }

        if let Some(comments) = &mut self.comments {
            let content = unsafe { self.source.get_unchecked(content_start..content_end) };
            comments.push(Comment::Line(LineComment {
                content,
                span: Span { start, end },
            }));
        }
    }

    fn scan_ident_sequence(&mut self) -> PResult<Ident<'s>> {
        let start;
        let mut end;
        match self.peek_one_char() {
            Some((i, '-')) => {
                start = i;
                self.state.chars.next();
                if let Some((i, c)) = self.state.chars.next() {
                    debug_assert!(
                        c.is_ascii_alphabetic()
                            || c == '-'
                            || c == '_'
                            || !c.is_ascii()
                            || c == '\\'
                    );
                    end = i + c.len_utf8();
                } else {
                    return Err(self.build_eof_error());
                }
            }
            Some((i, c)) if c.is_ascii_alphabetic() || c == '_' || !c.is_ascii() => {
                start = i;
                self.state.chars.next();
                end = i + c.len_utf8();
            }
            Some((i, '\\')) => {
                start = i;
                end = self.scan_escape()?;
            }
            _ => {
                return Err(self.build_eof_error());
            }
        }

        while let Some((i, c)) = self.peek_one_char() {
            if c.is_ascii_alphanumeric() || c == '-' || c == '_' || !c.is_ascii() {
                self.state.chars.next();
            } else if c == '\\' {
                self.scan_escape()?;
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

    fn scan_escape(&mut self) -> PResult<usize> {
        self.state.chars.next(); // consume `\\`
        match self.state.chars.next() {
            Some((i, c)) if c.is_ascii_hexdigit() => {
                let mut count: usize = 1;
                let mut end = i + 1;
                while let Some((i, c)) = self.peek_one_char() {
                    if c.is_ascii_hexdigit() && count < 6 {
                        count += 1;
                        self.state.chars.next();
                    } else {
                        // according to https://www.w3.org/TR/css-syntax-3/#hex-digit,
                        // consume a whitespace
                        if c.is_ascii_whitespace() {
                            self.state.chars.next();
                            end = i + 1;
                        } else {
                            end = i;
                        }
                        break;
                    }
                }
                Ok(end)
            }
            Some((i, c)) => Ok(i + c.len_utf8()),
            None => Err(self.build_eof_error()),
        }
    }

    fn scan_number(&mut self) -> PResult<Number<'s>> {
        let start;
        let mut end = 0;

        let is_start_with_dot;
        if let Some((i, c)) = self.state.chars.next() {
            start = i;
            if c.is_ascii_digit() {
                is_start_with_dot = false;
                end = i + 1;
            } else if c == '+' || c == '-' {
                is_start_with_dot = if let Some((_, '.')) = self.state.chars.peek() {
                    self.state.chars.next();
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
            return Err(self.build_eof_error());
        }

        if is_start_with_dot {
            while let Some((i, c)) = self.peek_one_char() {
                if c.is_ascii_digit() {
                    self.state.chars.next();
                } else {
                    end = i;
                    break;
                }
            }
        } else {
            while let Some((i, c)) = self.peek_one_char() {
                if c.is_ascii_digit() {
                    self.state.chars.next();
                } else {
                    end = i;
                    break;
                }
            }
            if let Some((_, '.')) = self.state.chars.peek() {
                // bump '.'
                self.state.chars.next();
                while let Some((i, c)) = self.peek_one_char() {
                    if c.is_ascii_digit() {
                        self.state.chars.next();
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
                self.state.chars.next();

                if let Some((_, '-' | '+')) = self.state.chars.peek() {
                    self.state.chars.next();
                }

                while let Some((i, c)) = self.state.chars.clone().peek() {
                    if c.is_ascii_digit() {
                        self.state.chars.next();
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

    fn scan_dimension_or_percentage(&mut self, number: Number<'s>) -> PResult<Token<'s>> {
        match self.peek_two_chars() {
            Some((_, '-', c))
                if c.is_ascii_alphabetic()
                    || c == '-'
                    || c == '_'
                    || c == '\\'
                    || !c.is_ascii() =>
            {
                self.scan_dimension(number)
            }
            _ => match self.state.chars.peek() {
                Some((_, c))
                    if c.is_ascii_alphabetic() || *c == '_' || *c == '\\' || !c.is_ascii() =>
                {
                    self.scan_dimension(number)
                }
                Some((_, '%')) => self.scan_percentage(number),
                _ => Ok(Token::Number(number)),
            },
        }
    }

    fn scan_dimension(&mut self, value: Number<'s>) -> PResult<Token<'s>> {
        let unit = self.scan_ident_sequence()?;
        let span = Span {
            start: value.span.start,
            end: unit.span.end,
        };
        Ok(Token::Dimension(Dimension { value, unit, span }))
    }

    fn scan_percentage(&mut self, value: Number<'s>) -> PResult<Token<'s>> {
        let start = value.span.start;
        let (i, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '%');
        Ok(Token::Percentage(Percentage {
            value,
            span: Span { start, end: i + 1 },
        }))
    }

    fn scan_string(&mut self) -> PResult<Str<'s>> {
        // '\'' or '"' is checked (but not consumed) before
        let (start, quote) = self.state.chars.next().unwrap();

        let end;
        loop {
            match self.state.chars.next() {
                Some((i, '\n')) => {
                    return Err(Error {
                        kind: ErrorKind::UnexpectedLinebreak,
                        span: Span {
                            start: i,
                            end: i + 1,
                        },
                    })
                }
                Some((i, c)) if c == quote => {
                    end = i + c.len_utf8();
                    break;
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
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

    fn scan_ident_or_url(&mut self) -> PResult<Token<'s>> {
        let ident = self.scan_ident_sequence()?;
        match self.state.chars.peek() {
            Some((_, '(')) if ident.name.eq_ignore_ascii_case("url") => {
                self.scan_url(ident.span.start).map(Token::Url)
            }
            _ => Ok(Token::Ident(ident)),
        }
    }

    fn scan_url(&mut self, start: usize) -> PResult<Url<'s>> {
        let (_, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '(');

        self.skip_ws();
        if let Some((i, '\'' | '"')) = self.state.chars.peek() {
            Ok(Url {
                raw: None,
                span: Span { start, end: *i },
            })
        } else {
            // ')' is consumed
            let raw = self.scan_url_raw()?;
            let span = Span {
                start,
                end: raw.span.end,
            };
            Ok(Url {
                raw: Some(raw),
                span,
            })
        }
    }

    fn scan_url_raw(&mut self) -> PResult<UrlRaw<'s>> {
        let start = self.current_offset();
        let mut end = self.source.len();
        for (i, c) in self.state.chars.by_ref() {
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

    fn scan_hash(&mut self) -> PResult<Token<'s>> {
        let (start, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '#');

        let mut end;
        match self.state.chars.next() {
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
                });
            }
            None => {
                return Err(self.build_eof_error());
            }
        }
        while let Some((i, c)) = self.peek_one_char() {
            if c.is_ascii_alphanumeric() || c == '-' || c == '_' || !c.is_ascii() || c == '\\' {
                self.state.chars.next();
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

    fn scan_dollar_var(&mut self) -> PResult<Token<'s>> {
        let (start, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '$');
        let ident = self.scan_ident_sequence()?;
        let span = Span {
            start,
            end: ident.span.end,
        };
        Ok(Token::DollarVar(DollarVar { ident, span }))
    }

    fn scan_at_lbrace_var(&mut self) -> PResult<Token<'s>> {
        let (start, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '@');
        let (_, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '{');

        let ident = self.scan_ident_sequence()?;
        match self.state.chars.next() {
            Some((i, '}')) => Ok(Token::AtLBraceVar(AtLBraceVar {
                ident,
                span: Span { start, end: i + 1 },
            })),
            Some((i, c)) => Err(Error {
                kind: ErrorKind::ExpectRightBraceForLessVar,
                span: Span {
                    start: i,
                    end: i + c.len_utf8(),
                },
            }),
            None => Err(self.build_eof_error()),
        }
    }

    fn scan_at_keyword(&mut self) -> PResult<Token<'s>> {
        let (start, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '@');
        let ident = self.scan_ident_sequence()?;
        let span = Span {
            start,
            end: ident.span.end,
        };
        Ok(Token::AtKeyword(AtKeyword { ident, span }))
    }

    fn scan_punc(&mut self) -> Option<Token<'s>> {
        match self.peek_two_chars() {
            Some((i, ':', ':')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::ColonColon(ColonColon {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '|', '|')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::BarBar(BarBar {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '~', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::TildeEqual(TildeEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '|', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::BarEqual(BarEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '^', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::CaretEqual(CaretEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '$', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::DollarEqual(DollarEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '*', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::AsteriskEqual(AsteriskEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '#', '{')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::HashLBrace(HashLBrace {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '=', '=')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::EqualEqual(EqualEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '!', '=')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::ExclamationEqual(ExclamationEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '>', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::GreaterThanEqual(GreaterThanEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '<', '=')) => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::LessThanEqual(LessThanEqual {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            Some((i, '+', '_')) if self.syntax == Syntax::Less => {
                self.state.chars.next();
                self.state.chars.next();
                Some(Token::PlusUnderscore(PlusUnderscore {
                    span: Span {
                        start: i,
                        end: i + 2,
                    },
                }))
            }
            _ => match self.state.chars.next() {
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
                Some((i, '<')) => Some(Token::LessThan(LessThan {
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
                Some((i, '-')) => Some(Token::Minus(Minus {
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
                Some((i, '%')) => Some(Token::Percent(Percent {
                    span: Span {
                        start: i,
                        end: i + 1,
                    },
                })),
                Some((i, '#')) => Some(Token::NumberSign(NumberSign {
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
