use crate::{
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    pos::Span,
};
use std::{cmp::Ordering, iter::Peekable, str::CharIndices};
pub(crate) use symbol::TokenSymbol;
use token::*;
pub use token::{Token, TokenWithSpan};

mod convert;
mod misc;
mod symbol;
pub mod token;

#[derive(Clone)]
pub(crate) struct TokenizerState<'s> {
    chars: Peekable<CharIndices<'s>>,
    /// Indentation size per indentation level. SASS requires consistent indentation level for a block,
    /// CSS/LESS/SCSS are completely insensible to the indentation
    indent_stops: Vec<u16>,
    /// Dedent doesn't consume anything unless it's the last one required by indentation level,
    /// so we put em here
    indent_stack: Vec<TokenWithSpan<'s>>
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
                indent_stops: vec![0],
                indent_stack: Vec::new()
            },
        }
    }

    #[inline]
    pub fn bump(&mut self) -> PResult<TokenWithSpan<'s>> {
        self.skip_ws_comment_or_indent()?;

        if let Some(indent) =  self.state.indent_stack.pop(){
            Ok(indent)
        } else {
            self.next()
        }
    }

    #[inline]
    pub fn bump_without_ws_or_comments(&mut self) -> PResult<TokenWithSpan<'s>> {
        self.next()
    }

    pub fn current_offset(&mut self) -> usize {
        if let Some((offset, _)) = self.state.chars.peek() {
            *offset
        } else {
            self.source.len()
        }
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

    fn next(&mut self) -> PResult<TokenWithSpan<'s>> {
        // detect frequent tokens here, but DO NOT add too many and don't forget to do profiling
        match self.state.chars.peek() {
            Some((_, c)) if is_start_of_ident(*c) && c != &'-' => return self.scan_ident(),
            Some((_, c)) if c.is_ascii_digit() => {
                let (number, span) = self.scan_number()?;
                return self.scan_dimension_or_percentage(number, span);
            }
            Some((start, '{')) => {
                let token = TokenWithSpan {
                    token: Token::LBrace(LBrace {}),
                    span: Span {
                        start: *start,
                        end: start + 1,
                    },
                };
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
            (Some((_, '@')), Some((_, c))) if is_start_of_ident(c) => self.scan_at_keyword(),
            (Some((start, '-')), Some((_, '-'))) => {
                if matches!(chars.peek(), Some((_, '>'))) {
                    self.scan_cdc(start)
                } else {
                    self.scan_ident()
                }
            }
            (Some((_, '-')), Some((_, c))) if is_start_of_ident(c) => self.scan_ident(),
            (Some((_, '.' | '+' | '-')), Some((_, c))) if c.is_ascii_digit() => {
                let (number, span) = self.scan_number()?;
                self.scan_dimension_or_percentage(number, span)
            }
            (Some((_, '-' | '+')), Some((_, '.'))) if matches!(chars.peek(), Some((_, c)) if c.is_ascii_digit()) =>
            {
                let (number, span) = self.scan_number()?;
                self.scan_dimension_or_percentage(number, span)
            }
            (Some((_, '$')), Some((_, c))) if is_start_of_ident(c) => self.scan_dollar_var(),
            (Some((_, '-')), Some((_, '#')))
                if matches!(self.syntax, Syntax::Scss | Syntax::Sass)
                    && matches!(chars.peek(), Some((_, '{'))) =>
            {
                self.scan_sass_single_hyphen_as_ident()
            }
            (Some((_, '@' | '$')), Some((_, '{')))
                if self.syntax == Syntax::Less
                    && matches!(chars.peek(), Some((_, c)) if is_start_of_ident(*c)) =>
            {
                self.scan_less_lbrace_var()
            }
            (Some((_, '`')), _) if self.syntax == Syntax::Less => self.scan_backtick_code(),
            (Some(..), ..) => self.scan_punc(),
            (None, ..) => {
                let offset = self.current_offset();
                Ok(TokenWithSpan {
                    token: Token::Eof(Eof {}),
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                })
            }
        }
    }

    fn skip_ws_comment_or_indent(&mut self) -> PResult<()> {
        while let Some((_, c)) = self.state.chars.peek() {
            match c {
                '/' => {
                    let mut chars = self.state.chars.clone();
                    chars.next();
                    match chars.peek() {
                        Some((_, '*')) => self.scan_block_comment(),
                        Some((_, '/')) if self.syntax != Syntax::Css => self.scan_line_comment(),
                        _ => break,
                    }
                },
                _ if c.is_ascii_whitespace() => {
                    if self.syntax == Syntax::Sass {
                        if let Some(indent) = self.scan_indent() {
                            self.state.indent_stack.push(indent?);
                        }
                    } else {
                        self.skip_ws();
                    }
                },
                _ => break
            }
        }

        Ok(())
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

    fn scan_indent(&mut self) -> Option<PResult<TokenWithSpan<'s>>> {
        debug_assert_eq!(self.syntax, Syntax::Sass);
        let mut start = None;

        // Shall not actually consume self.state.chars untill:
        // - indent is met
        // - EOF is met
        // - Linebreak is met
        // - indent/dedent start is None
        // "consuming" is move source_mirror -> self.state.chars
        let mut source_mirror = self.state.chars.clone();

        while let Some((end, c)) = source_mirror.peek().copied() {
            if c.is_ascii_whitespace() {
                let (i, c) = source_mirror.next()?;
                if c == '\n' || c == '\r' && matches!(source_mirror.peek(), Some((_, '\n'))) {
                    start = Some(i + 1);
                }
            } else {
                return if let Some(start) = start {
                    let len = (end - start) as u16;
                    let span = Span { start, end };

                    if let Some(current_level) = self.state.indent_stops.last() {
                        match len.cmp(current_level) {
                            Ordering::Greater => {
                                self.state.indent_stops.push(len);
                                self.state.chars = source_mirror;
                                Some(Ok(TokenWithSpan {
                                    token: Token::Indent(Indent {}),
                                    span,
                                }))
                            }
                            Ordering::Equal => {
                                self.state.chars = source_mirror;
                                Some(Ok(TokenWithSpan {
                                    token: Token::Linebreak(Linebreak {}),
                                    span,
                                }))
                            }
                            Ordering::Less => {
                                let matching_level = self
                                    .state
                                    .indent_stops
                                    .iter()
                                    .enumerate()
                                    .find(|(_, l)| **l == len);

                                if matching_level.is_some() {
                                    self.state
                                        .indent_stops
                                        .truncate(self.state.indent_stops.len() - 1);

                                    Some(Ok(TokenWithSpan {
                                        token: Token::Dedent(Dedent {}),
                                        span,
                                    }))
                                } else {
                                    self.state.chars = source_mirror;
                                    Some(Err(Error {
                                        kind: ErrorKind::InconsistentIndentation,
                                        span,
                                    }))
                                }
                            }
                        }
                    } else {
                        self.state.chars = source_mirror;
                        self.state.indent_stops.push(len);
                        Some(Ok(TokenWithSpan {
                            token: Token::Indent(Indent {}),
                            span,
                        }))
                    }
                } else {
                    self.state.chars = source_mirror;
                    None
                };
            }
        }

        self.state.chars = source_mirror;
        let offset = self.current_offset();
        Some(Ok(TokenWithSpan {
            token: Token::Eof(Eof {}),
            span: Span {
                start: offset,
                end: offset,
            },
        }))
    }

    fn scan_block_comment(&mut self) {
        let (start, c) = self.state.chars.next().unwrap();
        debug_assert_eq!(c, '/');
        self.state.chars.next();

        let content_end;
        let end;
        loop {
            match self.state.chars.next() {
                Some((_, '*')) => {
                    if let Some((i, '/')) = self.state.chars.peek() {
                        content_end = i - 1;
                        end = i + 1;
                        self.state.chars.next();
                        break;
                    }
                }
                Some(..) => {}
                None => {
                    content_end = self.source.len();
                    end = content_end;
                    break;
                }
            }
        }

        if let Some(comments) = &mut self.comments {
            let content = unsafe { self.source.get_unchecked(start + 2..content_end) };
            comments.push(Comment {
                content,
                kind: CommentKind::Block,
                span: Span { start, end },
            });
        }
    }

    fn scan_line_comment(&mut self) {
        let (start, c) = self.state.chars.next().unwrap();
        debug_assert_eq!(c, '/');
        self.state.chars.next();

        let end;
        loop {
            match self.state.chars.peek() {
                Some((_, '\r')) => {
                    self.state.chars.next();
                    if let Some((i, '\n')) = self.state.chars.peek() {
                        end = i - 1;
                        if self.syntax != Syntax::Sass {
                            self.state.chars.next();
                        }
                        break;
                    }
                }
                Some((i, '\n')) => {
                    end = *i;
                    if self.syntax != Syntax::Sass {
                        self.state.chars.next();
                    }
                    break;
                }
                Some(..) => {
                    self.state.chars.next();
                }
                None => {
                    end = self.source.len();
                    break;
                }
            }
        }

        if let Some(comments) = &mut self.comments {
            let content = unsafe { self.source.get_unchecked(start + 2..end) };
            comments.push(Comment {
                content,
                kind: CommentKind::Line,
                span: Span { start, end },
            });
        }
    }

    pub(crate) fn scan_ident_sequence(&mut self) -> PResult<(Ident<'s>, Span)> {
        let start;
        let end;
        let mut escaped = false;
        match self.state.chars.peek() {
            Some((i, c)) if c.is_ascii_alphabetic() || *c == '_' || !c.is_ascii() => {
                start = *i;
                self.state.chars.next();
            }
            Some((i, '-')) => {
                start = *i;
                self.state.chars.next();
                if let Some((_, c)) = self.state.chars.next() {
                    debug_assert!(is_start_of_ident(c));
                } else {
                    return Err(self.build_eof_error());
                }
            }
            Some((i, '\\')) => {
                escaped = true;
                start = *i;
                self.scan_escape(/* backslash_consumed */ false)?;
            }
            _ => unreachable!(),
        }

        loop {
            match self.state.chars.peek() {
                Some((_, c))
                    if c.is_ascii_alphanumeric() || *c == '-' || *c == '_' || !c.is_ascii() =>
                {
                    self.state.chars.next();
                }
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ false)?;
                }
                Some((i, _)) => {
                    end = *i;
                    break;
                }
                None => {
                    end = self.source.len();
                    break;
                }
            }
        }

        debug_assert!(start < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok((Ident { raw, escaped }, Span { start, end }))
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

    fn scan_number(&mut self) -> PResult<(Number<'s>, Span)> {
        let start;
        let mut end;

        let is_start_with_dot;
        match self.state.chars.next() {
            Some((i, c)) if c.is_ascii_digit() => {
                start = i;
                is_start_with_dot = false;
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

        loop {
            match self.state.chars.peek() {
                Some((_, c)) if c.is_ascii_digit() => {
                    self.state.chars.next();
                }
                Some((i, _)) => {
                    end = *i;
                    break;
                }
                None => {
                    end = self.source.len();
                    break;
                }
            }
        }
        if !is_start_with_dot {
            let chars = self.state.chars.clone();
            match self.state.chars.peek() {
                // next token can be a `DotDotDot` token
                Some((_, '.')) if !matches!(chars.clone().nth(1), Some((_, '.'))) => {
                    // bump '.'
                    self.state.chars.next();
                    loop {
                        match self.state.chars.peek() {
                            Some((_, c)) if c.is_ascii_digit() => {
                                self.state.chars.next();
                            }
                            Some((i, _)) => {
                                end = *i;
                                break;
                            }
                            None => {
                                end = self.source.len();
                                break;
                            }
                        }
                    }
                }
                _ => {}
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

                loop {
                    match self.state.chars.clone().peek() {
                        Some((_, c)) if c.is_ascii_digit() => {
                            self.state.chars.next();
                        }
                        Some((i, _)) => {
                            end = *i;
                            break;
                        }
                        None => {
                            end = self.source.len();
                            break;
                        }
                    }
                }
            }
            _ => {}
        }

        debug_assert!(start < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok((Number { raw }, Span { start, end }))
    }

    fn scan_dimension_or_percentage(
        &mut self,
        number: Number<'s>,
        span: Span,
    ) -> PResult<TokenWithSpan<'s>> {
        let mut chars = self.state.chars.clone();
        match (chars.next(), chars.next()) {
            (Some((_, '-')), Some((_, c))) if is_start_of_ident(c) => {
                self.scan_dimension(number, span)
            }
            (Some((_, c)), ..) if c != '-' && is_start_of_ident(c) => {
                self.scan_dimension(number, span)
            }
            (Some((_, '%')), ..) => self.scan_percentage(number, span),
            _ => Ok(TokenWithSpan {
                token: Token::Number(number),
                span,
            }),
        }
    }

    fn scan_dimension(
        &mut self,
        value: Number<'s>,
        value_span: Span,
    ) -> PResult<TokenWithSpan<'s>> {
        let (unit, unit_span) = self.scan_ident_sequence()?;
        Ok(TokenWithSpan {
            token: Token::Dimension(Dimension { value, unit }),
            span: Span {
                start: value_span.start,
                end: unit_span.end,
            },
        })
    }

    fn scan_percentage(&mut self, value: Number<'s>, span: Span) -> PResult<TokenWithSpan<'s>> {
        self.state.chars.next();
        Ok(TokenWithSpan {
            token: Token::Percentage(Percentage { value }),
            span: Span {
                start: span.start,
                end: span.end + 1,
            },
        })
    }

    pub(crate) fn scan_string_only(&mut self) -> PResult<(Str<'s>, Span)> {
        let (start, quote) = match self.state.chars.next() {
            Some((index, c @ '\'' | c @ '"')) => (index, c),
            Some((index, _)) => {
                return Err(Error {
                    kind: ErrorKind::ExpectString,
                    span: Span {
                        start: index,
                        end: index + 1,
                    },
                })
            }
            None => return Err(self.build_eof_error()),
        };

        let end;
        let mut escaped = false;
        loop {
            match self.state.chars.next() {
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((i, c)) if c == quote => {
                    end = i + 1;
                    break;
                }
                Some((end, '\n')) => {
                    return Err(Error {
                        kind: ErrorKind::UnterminatedString,
                        span: Span { start, end },
                    });
                }
                Some(..) => {}
                None => {
                    return Err(Error {
                        kind: ErrorKind::UnterminatedString,
                        span: Span {
                            start,
                            end: self.source.len(),
                        },
                    });
                }
            }
        }

        debug_assert!(start + 1 < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok((Str { raw, escaped }, Span { start, end }))
    }

    fn scan_string_or_template(&mut self) -> PResult<TokenWithSpan<'s>> {
        // '\'' or '"' is checked (but not consumed) before
        let (start, quote) = self.state.chars.next().unwrap();

        let end;
        let mut escaped = false;
        loop {
            match self.state.chars.next() {
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((i, c)) if c == quote => {
                    end = i + 1;
                    break;
                }
                Some((end, c @ '#' | c @ '@' | c @ '$'))
                    if self.is_start_of_interpolation_in_str_template(c) =>
                {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok(TokenWithSpan {
                        token: Token::StrTemplate(StrTemplate {
                            raw,
                            escaped,
                            head: true,
                            tail: false,
                        }),
                        span,
                    });
                }
                Some((end, '\n')) => {
                    return Err(Error {
                        kind: ErrorKind::UnterminatedString,
                        span: Span { start, end },
                    });
                }
                Some(..) => {}
                None => {
                    return Err(Error {
                        kind: ErrorKind::UnterminatedString,
                        span: Span {
                            start,
                            end: self.source.len(),
                        },
                    });
                }
            }
        }

        debug_assert!(start + 1 < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok(TokenWithSpan {
            token: Token::Str(Str { raw, escaped }),
            span: Span { start, end },
        })
    }

    pub(crate) fn scan_string_template(&mut self, quote: char) -> PResult<(StrTemplate<'s>, Span)> {
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
                    return Ok((
                        StrTemplate {
                            raw,
                            escaped,
                            head: false,
                            tail: true,
                        },
                        span,
                    ));
                }
                Some((end, c @ '#' | c @ '@' | c @ '$'))
                    if self.is_start_of_interpolation_in_str_template(c) =>
                {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok((
                        StrTemplate {
                            raw,
                            escaped,
                            head: false,
                            tail: false,
                        },
                        span,
                    ));
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
            }
        }
    }

    fn is_start_of_interpolation_in_str_template(&mut self, c: char) -> bool {
        match self.syntax {
            Syntax::Css => false,
            Syntax::Scss | Syntax::Sass => {
                c == '#' && matches!(self.state.chars.peek(), Some((_, '{')))
            }
            Syntax::Less => {
                (c == '@' || c == '$')
                    && matches!(self.peek_two_chars(), Some((_, '{', second)) if is_start_of_ident(second))
            }
        }
    }

    fn scan_ident(&mut self) -> PResult<TokenWithSpan<'s>> {
        self.scan_ident_sequence()
            .map(|(ident, span)| TokenWithSpan {
                token: Token::Ident(ident),
                span,
            })
    }

    pub(crate) fn scan_ident_template(&mut self) -> PResult<Option<(Ident<'s>, Span)>> {
        let start = self.current_offset();
        let mut escaped = false;

        let end;
        loop {
            match self.state.chars.peek() {
                Some((_, c))
                    if c.is_ascii_alphanumeric() || *c == '-' || *c == '_' || !c.is_ascii() =>
                {
                    self.state.chars.next();
                }
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ false)?;
                }
                Some((i, _)) => {
                    end = *i;
                    break;
                }
                None => {
                    end = self.source.len();
                    break;
                }
            }
        }
        if end > start {
            let raw = unsafe { self.source.get_unchecked(start..end) };
            Ok(Some((Ident { escaped, raw }, Span { start, end })))
        } else {
            Ok(None)
        }
    }

    fn scan_sass_single_hyphen_as_ident(&mut self) -> PResult<TokenWithSpan<'s>> {
        debug_assert!(matches!(self.syntax, Syntax::Scss | Syntax::Sass));
        match self.state.chars.next() {
            Some((start, c)) => {
                debug_assert_eq!(c, '-');
                Ok(TokenWithSpan {
                    token: Token::Ident(Ident {
                        escaped: false,
                        raw: "-",
                    }),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                })
            }
            None => Err(self.build_eof_error()),
        }
    }

    pub(crate) fn scan_url_raw_or_template(&mut self) -> PResult<TokenWithSpan<'s>> {
        self.skip_ws();
        let start = self.current_offset();
        let end;
        let mut escaped = false;
        loop {
            match self.state.chars.next() {
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
                    return Ok(TokenWithSpan {
                        token: Token::UrlTemplate(UrlTemplate {
                            raw,
                            escaped,
                            tail: false,
                        }),
                        span,
                    });
                }
                Some((i, c)) if c.is_ascii_whitespace() => {
                    self.skip_ws();
                    match self.state.chars.next() {
                        Some((_, ')')) => {
                            end = i;
                            break;
                        }
                        Some((i, c)) => {
                            return Err(Error {
                                kind: ErrorKind::InvalidUrl,
                                span: Span {
                                    start: i,
                                    end: i + c.len_utf8(),
                                },
                            })
                        }
                        None => return Err(self.build_eof_error()),
                    }
                }
                Some((i, '(' | '"' | '\'')) => {
                    return Err(Error {
                        kind: ErrorKind::InvalidUrl,
                        span: Span {
                            start: i,
                            end: i + 1,
                        },
                    });
                }
                Some(..) => {}
                None => return Err(self.build_eof_error()),
            }
        }

        debug_assert!(start <= end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        let span = Span { start, end };
        Ok(TokenWithSpan {
            token: Token::UrlRaw(UrlRaw { raw, escaped }),
            span,
        })
    }

    pub(crate) fn scan_url_template(&mut self) -> PResult<(UrlTemplate<'s>, Span)> {
        let start = self.current_offset();
        let mut escaped = false;
        loop {
            match self.state.chars.next() {
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ true)?;
                }
                Some((end, ')')) => {
                    debug_assert!(start <= end);

                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok((
                        UrlTemplate {
                            raw,
                            escaped,
                            tail: true,
                        },
                        span,
                    ));
                }
                Some((end, '#')) if self.is_start_of_interpolation_in_url_template() => {
                    let raw = unsafe { self.source.get_unchecked(start..end) };
                    let span = Span { start, end };
                    return Ok((
                        UrlTemplate {
                            raw,
                            escaped,
                            tail: false,
                        },
                        span,
                    ));
                }
                Some((end, c)) if c.is_ascii_whitespace() => {
                    self.skip_ws();
                    match self.state.chars.next() {
                        Some((_, ')')) => {
                            return Ok((
                                UrlTemplate {
                                    raw: unsafe { self.source.get_unchecked(start..end) },
                                    escaped,
                                    tail: true,
                                },
                                Span { start, end },
                            ));
                        }
                        Some((i, c)) => {
                            return Err(Error {
                                kind: ErrorKind::InvalidUrl,
                                span: Span {
                                    start: i,
                                    end: i + c.len_utf8(),
                                },
                            });
                        }
                        None => return Err(self.build_eof_error()),
                    }
                }
                Some((i, '(' | '"' | '\'')) => {
                    return Err(Error {
                        kind: ErrorKind::InvalidUrl,
                        span: Span {
                            start: i,
                            end: i + 1,
                        },
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

    fn scan_hash(&mut self) -> PResult<TokenWithSpan<'s>> {
        let (start, c) = self.state.chars.next().unwrap();
        debug_assert_eq!(c, '#');

        let end;
        let mut escaped = false;
        match self.state.chars.next() {
            Some((_, c)) if c.is_ascii_alphanumeric() || c == '-' || c == '_' || !c.is_ascii() => {}
            Some((_, '\\')) => {
                escaped = true;
                self.scan_escape(/* backslash_consumed */ true)?;
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
        loop {
            match self.state.chars.peek() {
                Some((_, c))
                    if c.is_ascii_alphanumeric() || *c == '-' || *c == '_' || !c.is_ascii() =>
                {
                    self.state.chars.next();
                }
                Some((_, '\\')) => {
                    escaped = true;
                    self.scan_escape(/* backslash_consumed */ false)?;
                }
                Some((i, _)) => {
                    end = *i;
                    break;
                }
                None => {
                    end = self.source.len();
                    break;
                }
            }
        }

        debug_assert!(end > start + 1);
        let raw = unsafe { self.source.get_unchecked(start + 1..end) };
        Ok(TokenWithSpan {
            token: Token::Hash(Hash { escaped, raw }),
            span: Span { start, end },
        })
    }

    fn scan_dollar_var(&mut self) -> PResult<TokenWithSpan<'s>> {
        let (start, c) = self.state.chars.next().expect("expect char `$`");
        debug_assert_eq!(c, '$');
        let (ident, span) = self.scan_ident_sequence()?;
        Ok(TokenWithSpan {
            token: Token::DollarVar(DollarVar { ident }),
            span: Span {
                start,
                end: span.end,
            },
        })
    }

    fn scan_less_lbrace_var(&mut self) -> PResult<TokenWithSpan<'s>> {
        let (start, first_char) = self.state.chars.next().expect("expect char `@` or `$`");
        debug_assert!(matches!(first_char, '@' | '$'));
        let (_, c) = self.state.chars.next().expect("expect char `{`");
        debug_assert_eq!(c, '{');

        let (ident, _) = self.scan_ident_sequence()?;
        match self.state.chars.next() {
            Some((i, '}')) => {
                let span = Span { start, end: i + 1 };
                if first_char == '@' {
                    Ok(TokenWithSpan {
                        token: Token::AtLBraceVar(AtLBraceVar { ident }),
                        span,
                    })
                } else {
                    Ok(TokenWithSpan {
                        token: Token::DollarLBraceVar(DollarLBraceVar { ident }),
                        span,
                    })
                }
            }
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

    fn scan_at_keyword(&mut self) -> PResult<TokenWithSpan<'s>> {
        let (start, c) = self.state.chars.next().expect("expect char `@`");
        debug_assert_eq!(c, '@');
        let (ident, span) = self.scan_ident_sequence()?;
        Ok(TokenWithSpan {
            token: Token::AtKeyword(AtKeyword { ident }),
            span: Span {
                start,
                end: span.end,
            },
        })
    }

    fn scan_backtick_code(&mut self) -> PResult<TokenWithSpan<'s>> {
        debug_assert!(self.syntax == Syntax::Less);

        // '`' is checked (but not consumed) before
        let (start, _) = self.state.chars.next().expect("expect char ```");

        let end;
        loop {
            match self.state.chars.next() {
                Some((i, '`')) => {
                    end = i + 1;
                    break;
                }
                Some(..) => {}
                None => {
                    return Err(self.build_eof_error());
                }
            }
        }

        debug_assert!(start + 1 < end);
        let raw = unsafe { self.source.get_unchecked(start..end) };
        Ok(TokenWithSpan {
            token: Token::BacktickCode(BacktickCode { raw }),
            span: Span { start, end },
        })
    }

    fn scan_punc(&mut self) -> PResult<TokenWithSpan<'s>> {
        match self.state.chars.next() {
            Some((start, '.')) => {
                if self.syntax != Syntax::Css
                    && matches!(
                        {
                            let mut chars = self.state.chars.clone();
                            (chars.next(), chars.next())
                        },
                        (Some((_, '.')), Some((_, '.')))
                    )
                {
                    self.state.chars.next();
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::DotDotDot(DotDotDot {}),
                        span: Span {
                            start,
                            end: start + 3,
                        },
                    })
                } else {
                    Ok(TokenWithSpan {
                        token: Token::Dot(Dot {}),
                        span: Span {
                            start,
                            end: start + 1,
                        },
                    })
                }
            }
            Some((start, ':')) => match self.state.chars.peek() {
                Some((_, ':')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::ColonColon(ColonColon {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Colon(Colon {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '}')) => Ok(TokenWithSpan {
                token: Token::RBrace(RBrace {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '(')) => Ok(TokenWithSpan {
                token: Token::LParen(LParen {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, ')')) => Ok(TokenWithSpan {
                token: Token::RParen(RParen {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '[')) => Ok(TokenWithSpan {
                token: Token::LBracket(LBracket {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, ']')) => Ok(TokenWithSpan {
                token: Token::RBracket(RBracket {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '/')) => Ok(TokenWithSpan {
                token: Token::Solidus(Solidus {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, ',')) => Ok(TokenWithSpan {
                token: Token::Comma(Comma {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, ';')) => Ok(TokenWithSpan {
                token: Token::Semicolon(Semicolon {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '>')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::GreaterThanEqual(GreaterThanEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::GreaterThan(GreaterThan {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '<')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::LessThanEqual(LessThanEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                Some((_, '!')) => {
                    let mut chars = self.state.chars.clone();
                    if {
                        chars.next();
                        matches!(
                            (chars.next(), chars.peek()),
                            (Some((_, '-')), Some((_, '-')))
                        )
                    } {
                        self.state.chars.next();
                        self.state.chars.next();
                        self.state.chars.next();
                        Ok(TokenWithSpan {
                            token: Token::Cdo(Cdo {}),
                            span: Span {
                                start,
                                end: start + 4,
                            },
                        })
                    } else {
                        Ok(TokenWithSpan {
                            token: Token::LessThan(LessThan {}),
                            span: Span {
                                start,
                                end: start + 1,
                            },
                        })
                    }
                }
                _ => Ok(TokenWithSpan {
                    token: Token::LessThan(LessThan {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '+')) => match self.state.chars.peek() {
                Some((_, '_')) if self.syntax == Syntax::Less => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::PlusUnderscore(PlusUnderscore {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Plus(Plus {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '=')) => match self.state.chars.peek() {
                Some((_, '=')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::EqualEqual(EqualEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Equal(Equal {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '-')) => Ok(TokenWithSpan {
                token: Token::Minus(Minus {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '~')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::TildeEqual(TildeEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Tilde(Tilde {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '&')) => Ok(TokenWithSpan {
                token: Token::Ampersand(Ampersand {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '*')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::AsteriskEqual(AsteriskEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Asterisk(Asterisk {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '|')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::BarEqual(BarEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                Some((_, '|')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::BarBar(BarBar {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Bar(Bar {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '^')) => match self.state.chars.peek() {
                Some((_, '=')) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::CaretEqual(CaretEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
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
                    Ok(TokenWithSpan {
                        token: Token::DollarEqual(DollarEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
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
                    Ok(TokenWithSpan {
                        token: Token::ExclamationEqual(ExclamationEqual {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::Exclamation(Exclamation {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '?')) => Ok(TokenWithSpan {
                token: Token::Question(Question {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '#')) => match self.state.chars.peek() {
                Some((_, '{')) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    self.state.chars.next();
                    Ok(TokenWithSpan {
                        token: Token::HashLBrace(HashLBrace {}),
                        span: Span {
                            start,
                            end: start + 2,
                        },
                    })
                }
                _ => Ok(TokenWithSpan {
                    token: Token::NumberSign(NumberSign {}),
                    span: Span {
                        start,
                        end: start + 1,
                    },
                }),
            },
            Some((start, '%')) => Ok(TokenWithSpan {
                token: Token::Percent(Percent {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
            Some((start, '@')) if self.syntax != Syntax::Css => Ok(TokenWithSpan {
                token: Token::At(At {}),
                span: Span {
                    start,
                    end: start + 1,
                },
            }),
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
                Ok(TokenWithSpan {
                    token: Token::Eof(Eof {}),
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                })
            }
        }
    }

    #[cold]
    fn scan_cdc(&mut self, start: usize) -> PResult<TokenWithSpan<'s>> {
        self.state.chars.next();
        self.state.chars.next();
        self.state.chars.next();
        Ok(TokenWithSpan {
            token: Token::Cdc(Cdc {}),
            span: Span {
                start,
                end: start + 3,
            },
        })
    }

    #[inline]
    pub(crate) fn is_start_of_ident(&mut self) -> bool {
        match self.state.chars.peek() {
            Some((_, c)) if is_start_of_ident(*c) => true,
            Some((_, '-')) => {
                let mut chars = self.state.chars.clone();
                chars.next();
                matches!(chars.peek(), Some((_, c)) if is_start_of_ident(*c))
            }
            _ => false,
        }
    }

    pub(crate) fn is_start_of_url_string(&mut self) -> bool {
        self.skip_ws();
        matches!(self.state.chars.peek(), Some((_, '"' | '\'')))
    }
}

#[inline]
fn is_start_of_ident(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '-' || c == '_' || !c.is_ascii() || c == '\\'
}
