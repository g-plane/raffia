use crate::{
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    pos::Span,
};
use beef::Cow;
use std::{cmp::Ordering, iter::Peekable, str::CharIndices};
pub use token::Token;
use token::*;

mod convert;
pub mod token;

#[derive(Clone)]
pub(crate) struct TokenizerState<'s> {
    chars: Peekable<CharIndices<'s>>,
    indent_size: u16,
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

    #[inline]
    pub fn bump(&mut self) -> PResult<Token<'s>> {
        if let Some(indent) = self.skip_ws_or_comment() {
            Ok(indent)
        } else {
            self.next()
        }
    }

    #[inline]
    pub fn bump_without_ws_or_comments(&mut self) -> PResult<Token<'s>> {
        self.next()
    }

    pub fn peek(&mut self) -> PResult<Token<'s>> {
        let state = self.state.clone();
        let comments = self.comments.take();

        let token = self.bump();
        self.state = state;
        self.comments = comments;
        token
    }

    pub fn current_offset(&mut self) -> usize {
        if let Some((offset, _)) = self.state.chars.peek() {
            *offset
        } else {
            self.source.len()
        }
    }

    /// This should only be used when parsing selectors.
    #[inline]
    pub fn has_ws_or_comments(&mut self) -> bool {
        match self.state.chars.peek() {
            Some((_, c)) if c.is_ascii_whitespace() => true,
            Some((_, '/')) => {
                let mut chars = self.state.chars.clone();
                chars.next();
                matches!(chars.next(), Some((_, '*' | '/')))
            }
            _ => false,
        }
    }

    #[inline]
    fn peek_one_char(&mut self) -> Option<(usize, char)> {
        self.state.chars.peek().copied()
    }

    #[inline]
    fn peek_two_chars(&self) -> Option<(usize, char, char)> {
        let mut iter = self.state.chars.clone();
        iter.next()
            .zip(iter.next())
            .map(|((start, first), (_, second))| (start, first, second))
    }

    #[cold]
    fn build_eof_error(&mut self) -> Error {
        let offset = self.current_offset();
        Error {
            kind: ErrorKind::UnexpectedEof,
            span: Span {
                start: offset,
                end: offset,
            },
        }
    }

    fn next(&mut self) -> PResult<Token<'s>> {
        // detect frequent tokens here, but DO NOT add too many and don't forget to do profiling
        match self.state.chars.peek() {
            Some((_, c)) if is_start_of_ident(*c) && c != &'-' => return self.scan_ident_or_url(),
            Some((_, c)) if c.is_ascii_digit() => {
                let number = self.scan_number()?;
                return self.scan_dimension_or_percentage(number);
            }
            Some((start, '{')) => {
                let token = Token::LBrace(LBrace {
                    span: Span {
                        start: *start,
                        end: start + 1,
                    },
                });
                self.state.chars.next();
                return Ok(token);
            }
            _ => {}
        }
        let mut chars = self.state.chars.clone();
        match (chars.next(), chars.next()) {
            (Some((_, '#')), Some((_, c)))
                if c.is_ascii_alphanumeric()
                    || c == '-'
                    || c == '_'
                    || !c.is_ascii()
                    || c == '\\' =>
            {
                self.scan_hash()
            }
            (Some((_, '\'' | '"')), ..) => self.scan_string_or_template(),
            (Some((_, '-')), Some((_, c))) if is_start_of_ident(c) => self.scan_ident_or_url(),
            (Some((_, '.' | '+' | '-')), Some((_, c))) if c.is_ascii_digit() => {
                let number = self.scan_number()?;
                self.scan_dimension_or_percentage(number)
            }
            (Some((_, '-' | '+')), Some((_, '.'))) if matches!(chars.peek(), Some((_, c)) if c.is_ascii_digit()) =>
            {
                let number = self.scan_number()?;
                self.scan_dimension_or_percentage(number)
            }
            (Some((_, '@')), Some((_, c))) if is_start_of_ident(c) => self.scan_at_keyword(),
            (Some((_, '$')), Some((_, c)))
                if matches!(self.syntax, Syntax::Scss | Syntax::Sass) && is_start_of_ident(c) =>
            {
                self.scan_dollar_var()
            }
            (Some((_, '@')), Some((_, '{')))
                if self.syntax == Syntax::Less
                    && matches!(chars.peek(), Some((_, c)) if is_start_of_ident(*c)) =>
            {
                self.scan_at_lbrace_var()
            }
            (Some(..), ..) => self.scan_punc(),
            (None, ..) => {
                let offset = self.current_offset();
                Ok(Token::Eof(Eof {
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                }))
            }
        }
    }

    fn skip_ws_or_comment(&mut self) -> Option<Token<'s>> {
        let mut indent = None;
        loop {
            match self.state.chars.peek() {
                Some((_, c)) if c.is_ascii_whitespace() => {
                    if self.syntax == Syntax::Sass {
                        indent = self.scan_indent();
                    } else {
                        self.skip_ws();
                    }
                }
                Some((_, '/')) => {
                    let mut chars = self.state.chars.clone();
                    chars.next();
                    match chars.next() {
                        Some((_, '*')) => self.scan_block_comment(),
                        Some((_, '/')) if self.syntax != Syntax::Css => self.scan_line_comment(),
                        _ => break,
                    }
                }
                _ => break,
            }
        }
        indent
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
                    let len = (end - start) as u16;
                    let span = Span { start, end };
                    match len.cmp(&self.state.indent_size) {
                        Ordering::Greater => {
                            self.state.indent_size = len as u16;
                            Token::Indent(Indent { span })
                        }
                        Ordering::Less => {
                            self.state.indent_size = len as u16;
                            Token::Dedent(Dedent { span })
                        }
                        Ordering::Equal => Token::Linebreak(Linebreak { span }),
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
        let mut escaped = false;
        match self.state.chars.peek() {
            Some((i, c)) if c.is_ascii_alphabetic() || *c == '_' || !c.is_ascii() => {
                start = *i;
                end = start + c.len_utf8();
                self.state.chars.next();
            }
            Some((i, '-')) => {
                start = *i;
                self.state.chars.next();
                if let Some((i, c)) = self.state.chars.next() {
                    debug_assert!(is_start_of_ident(c));
                    end = i + c.len_utf8();
                } else {
                    return Err(self.build_eof_error());
                }
            }
            Some((i, '\\')) => {
                escaped = true;
                start = *i;
                end = self.scan_escape(/* backslash_consumed */ false)?;
            }
            _ => unreachable!(),
        }

        while let Some((i, c)) = self.state.chars.peek() {
            if c.is_ascii_alphanumeric() || *c == '-' || *c == '_' || !c.is_ascii() {
                self.state.chars.next();
            } else if c == &'\\' {
                escaped = true;
                self.scan_escape(/* backslash_consumed */ false)?;
            } else {
                end = *i;
                break;
            }
        }

        debug_assert!(start < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let span = Span { start, end };
        Ok(Ident {
            name: if escaped {
                handle_escape(raw).map_err(|kind| Error {
                    kind,
                    span: span.clone(),
                })?
            } else {
                Cow::from(raw)
            },
            raw,
            span,
        })
    }

    fn scan_escape(&mut self, backslash_consumed: bool) -> PResult<usize> {
        if !backslash_consumed {
            self.state.chars.next(); // consume `\\`
        }
        match self.state.chars.next() {
            Some((i, c)) if c.is_ascii_hexdigit() => {
                let mut count: usize = 1;
                let mut end = i + 1;
                while let Some((i, c)) = self.state.chars.peek() {
                    if c.is_ascii_hexdigit() && count < 6 {
                        count += 1;
                        self.state.chars.next();
                    } else {
                        // according to https://www.w3.org/TR/css-syntax-3/#hex-digit,
                        // consume a whitespace
                        if c.is_ascii_whitespace() {
                            end = i + 1;
                            self.state.chars.next();
                        } else {
                            end = *i;
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
        match self.state.chars.next() {
            Some((i, c)) if c.is_ascii_digit() => {
                start = i;
                is_start_with_dot = false;
                end = i + 1;
            }
            Some((i, '+' | '-')) => {
                start = i;
                is_start_with_dot = matches!(self.state.chars.next(), Some((_, '.')));
            }
            Some((i, '.')) => {
                start = i;
                is_start_with_dot = true;
            }
            _ => unreachable!(),
        }

        while let Some((i, c)) = self.state.chars.peek() {
            if c.is_ascii_digit() {
                self.state.chars.next();
            } else {
                end = *i;
                break;
            }
        }
        if !is_start_with_dot {
            if let Some((_, '.')) = self.state.chars.peek() {
                // bump '.'
                self.state.chars.next();
                while let Some((i, c)) = self.state.chars.peek() {
                    if c.is_ascii_digit() {
                        self.state.chars.next();
                    } else {
                        end = *i;
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

        debug_assert!(start < end);
        let span = Span { start, end };
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok(Number { raw, span })
    }

    fn scan_dimension_or_percentage(&mut self, number: Number<'s>) -> PResult<Token<'s>> {
        let mut chars = self.state.chars.clone();
        match (chars.next(), chars.next()) {
            (Some((_, '-')), Some((_, c))) if is_start_of_ident(c) => self.scan_dimension(number),
            (Some((_, c)), ..) if c != '-' && is_start_of_ident(c) => self.scan_dimension(number),
            (Some((_, '%')), ..) => self.scan_percentage(number),
            _ => Ok(Token::Number(number)),
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

    fn scan_string_or_template(&mut self) -> PResult<Token<'s>> {
        // '\'' or '"' is checked (but not consumed) before
        let (start, quote) = self.state.chars.next().unwrap();

        let end;
        let mut escaped = false;
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
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((i, c)) if c == quote => {
                    end = i + 1;
                    break;
                }
                Some((end, '#' | '@')) if self.is_start_of_interpolation_in_str_template() => {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok(Token::StrTemplate(StrTemplate {
                        raw,
                        escaped,
                        head: true,
                        tail: false,
                        span,
                    }));
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
            }
        }

        assert!(start + 1 < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let span = Span { start, end };
        Ok(Token::Str(Str { raw, escaped, span }))
    }

    pub(crate) fn scan_string_template(&mut self, quote: char) -> PResult<StrTemplate<'s>> {
        let start = self.current_offset();
        let end;
        let mut escaped = false;
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
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((i, c)) if c == quote => {
                    end = i + c.len_utf8();
                    debug_assert!(start < end);

                    let raw = unsafe { self.source.get_unchecked(start..i + 1) };
                    let span = Span { start, end };
                    return Ok(StrTemplate {
                        raw,
                        escaped,
                        head: false,
                        tail: true,
                        span,
                    });
                }
                Some((end, '#' | '@')) if self.is_start_of_interpolation_in_str_template() => {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok(StrTemplate {
                        raw,
                        escaped,
                        head: false,
                        tail: false,
                        span,
                    });
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
            }
        }
    }

    fn is_start_of_interpolation_in_str_template(&mut self) -> bool {
        match self.syntax {
            Syntax::Css => false,
            Syntax::Scss | Syntax::Sass => matches!(self.state.chars.peek(), Some((_, '{'))),
            Syntax::Less => {
                matches!(self.peek_two_chars(), Some((_, '{', second)) if is_start_of_ident(second))
            }
        }
    }

    fn scan_ident_or_url(&mut self) -> PResult<Token<'s>> {
        let ident = self.scan_ident_sequence()?;
        if ident.name.eq_ignore_ascii_case("url") {
            if let Some((_, '(')) = self.state.chars.peek() {
                self.scan_url(ident).map(Token::UrlPrefix)
            } else {
                Ok(Token::Ident(ident))
            }
        } else {
            Ok(Token::Ident(ident))
        }
    }

    fn scan_url(&mut self, ident: Ident<'s>) -> PResult<UrlPrefix<'s>> {
        let (i, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '(');

        self.skip_ws();
        let span = Span {
            start: ident.span.start,
            end: i + 1,
        };
        Ok(UrlPrefix {
            ident,
            is_raw: !matches!(self.state.chars.peek(), Some((_, '\'' | '"'))),
            span,
        })
    }

    pub(crate) fn scan_url_raw_or_template(&mut self) -> PResult<Token<'s>> {
        let start = self.current_offset();
        let end;
        let mut escaped = false;
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
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((i, ')')) => {
                    end = i;
                    break;
                }
                Some((end, '#')) if self.is_start_of_interpolation_in_url_template() => {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok(Token::UrlTemplate(UrlTemplate {
                        raw,
                        escaped,
                        tail: false,
                        span,
                    }));
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
            }
        }

        debug_assert!(start <= end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let span = Span { start, end };
        Ok(Token::UrlRaw(UrlRaw { raw, escaped, span }))
    }

    pub(crate) fn scan_url_template(&mut self) -> PResult<UrlTemplate<'s>> {
        let start = self.current_offset();
        let mut escaped = false;
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
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((end, ')')) => {
                    debug_assert!(start <= end);

                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok(UrlTemplate {
                        raw,
                        escaped,
                        tail: true,
                        span,
                    });
                }
                Some((end, '#')) if self.is_start_of_interpolation_in_url_template() => {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok(UrlTemplate {
                        raw,
                        escaped,
                        tail: false,
                        span,
                    });
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
            }
        }
    }

    fn is_start_of_interpolation_in_url_template(&mut self) -> bool {
        match self.syntax {
            Syntax::Css | Syntax::Less => false,
            Syntax::Scss | Syntax::Sass => matches!(self.state.chars.peek(), Some((_, '{'))),
        }
    }

    fn scan_hash(&mut self) -> PResult<Token<'s>> {
        let (start, c) = self
            .state
            .chars
            .next()
            .ok_or_else(|| self.build_eof_error())?;
        debug_assert_eq!(c, '#');

        let mut end;
        let mut escaped = false;
        match self.state.chars.next() {
            Some((i, c)) if c.is_ascii_alphanumeric() || c == '-' || c == '_' || !c.is_ascii() => {
                end = i + c.len_utf8();
            }
            Some((_, '\\')) => {
                escaped = true;
                end = self.scan_escape(/* backslash_consumed */ true)?;
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
        while let Some((i, c)) = self.state.chars.peek() {
            if c.is_ascii_alphanumeric() || *c == '-' || *c == '_' || !c.is_ascii() {
                self.state.chars.next();
            } else if c == &'\\' {
                escaped = true;
                self.scan_escape(/* backslash_consumed */ false)?;
            } else {
                end = *i;
                break;
            }
        }

        assert!(end > start + 1);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let raw_without_hash = unsafe { self.source.get_unchecked(start + 1..end) };
        let span = Span { start, end };
        Ok(Token::Hash(Hash {
            escaped,
            raw,
            raw_without_hash,
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

    fn scan_punc(&mut self) -> PResult<Token<'s>> {
        match self.state.chars.next() {
            Some((start, '.')) => Ok(Token::Dot(Dot {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, ':')) => match self.state.chars.peek() {
                Some((_, ':')) => {
                    self.state.chars.next();
                    Ok(Token::ColonColon(ColonColon {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Colon(Colon {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '}')) => Ok(Token::RBrace(RBrace {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, '(')) => Ok(Token::LParen(LParen {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, ')')) => Ok(Token::RParen(RParen {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, '[')) => Ok(Token::LBracket(LBracket {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, ']')) => Ok(Token::RBracket(RBracket {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, '/')) => Ok(Token::Solidus(Solidus {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, ',')) => Ok(Token::Comma(Comma {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, ';')) => Ok(Token::Semicolon(Semicolon {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, '>')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::GreaterThanEqual(GreaterThanEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::GreaterThan(GreaterThan {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '<')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::LessThanEqual(LessThanEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::LessThan(LessThan {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '+')) => match self.state.chars.peek() {
                Some((_, '_')) if self.syntax == Syntax::Less => {
                    self.state.chars.next();
                    Ok(Token::PlusUnderscore(PlusUnderscore {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Plus(Plus {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '=')) => match self.state.chars.peek() {
                Some((_, '=')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    self.state.chars.next();
                    Ok(Token::EqualEqual(EqualEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Equal(Equal {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '-')) => Ok(Token::Minus(Minus {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, '~')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::TildeEqual(TildeEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Tilde(Tilde {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '&')) => Ok(Token::Ampersand(Ampersand {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((start, '*')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::AsteriskEqual(AsteriskEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Asterisk(Asterisk {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '|')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::BarEqual(BarEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                Some((_, '|')) => {
                    self.state.chars.next();
                    Ok(Token::BarBar(BarBar {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Bar(Bar {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '^')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::CaretEqual(CaretEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Err(Error {
                    kind: ErrorKind::UnknownToken,
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '$')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(Token::DollarEqual(DollarEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Err(Error {
                    kind: ErrorKind::UnknownToken,
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '!')) => match self.state.chars.peek() {
                Some((_, '=')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    self.state.chars.next();
                    Ok(Token::ExclamationEqual(ExclamationEqual {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::Exclamation(Exclamation {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '#')) => match self.state.chars.peek() {
                Some((_, '{')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    self.state.chars.next();
                    Ok(Token::HashLBrace(HashLBrace {
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    }))
                }
                _ => Ok(Token::NumberSign(NumberSign {
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })),
            },
            Some((start, '%')) => Ok(Token::Percent(Percent {
                span: Span {
                    start,
                    end: start + 1,
                },
            })),
            Some((i, c)) if c.is_ascii_whitespace() => Err(Error {
                kind: ErrorKind::UnexpectedWhitespace,
                span: Span {
                    start: i,
                    end: i + 1,
                },
            }),
            Some((i, c)) => Err(Error {
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
        }
    }
}

pub(crate) fn handle_escape(s: &str) -> Result<Cow<str>, ErrorKind> {
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
                    escaped.push(char::from_u32(unicode).unwrap_or(char::REPLACEMENT_CHARACTER));
                }
                Some((_, c)) => escaped.push(c),
                None => return Err(ErrorKind::InvalidEscape),
            }
        } else {
            escaped.push(c);
        }
    }
    Ok(Cow::from(escaped))
}

#[inline]
fn is_start_of_ident(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '-' || c == '_' || !c.is_ascii() || c == '\\'
}
