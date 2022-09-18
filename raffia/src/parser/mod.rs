use self::state::ParserState;
use crate::{
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    pos::Span,
    tokenizer::{token::TokenWithSpan, Tokenizer},
};
pub use builder::ParserBuilder;

mod at_rule;
mod builder;
mod convert;
mod less;
mod macros;
mod sass;
mod selector;
mod state;
mod stmt;
mod value;

pub trait Parse<'cmt, 's: 'cmt>: Sized {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self>;
}

/// Create a parser with some source code, then parse it.
pub struct Parser<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    tokenizer: Tokenizer<'cmt, 's>,
    state: ParserState,
    recoverable_errors: Vec<Error>,
    cached_token: Option<TokenWithSpan<'s>>,
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    /// Create a parser with the given source code and specified syntax.
    /// If you need to control more options, please use [`ParserBuilder`].
    pub fn new(source: &'s str, syntax: Syntax) -> Self {
        Parser {
            source,
            syntax: syntax.clone(),
            tokenizer: Tokenizer::new(source, syntax, None),
            state: Default::default(),
            recoverable_errors: vec![],
            cached_token: None,
        }
    }

    /// Start to parse.
    pub fn parse<T>(&mut self) -> PResult<T>
    where
        T: Parse<'cmt, 's>,
    {
        T::parse(self)
    }

    /// Retrieve recoverable errors.
    #[inline]
    pub fn recoverable_errors(&self) -> &[Error] {
        &self.recoverable_errors
    }

    fn try_parse<R, F: Fn(&mut Self) -> PResult<R>>(&mut self, f: F) -> PResult<R> {
        let tokenizer_state = self.tokenizer.state.clone();
        let comments_count = if let Some(comments) = &self.tokenizer.comments {
            comments.len()
        } else {
            0
        };
        let recoverable_errors_count = self.recoverable_errors.len();
        let cached_token = self.cached_token.clone();
        let result = f(self);
        if result.is_err() {
            self.tokenizer.state = tokenizer_state;
            if let Some(comments) = &mut self.tokenizer.comments {
                comments.truncate(comments_count);
            }
            self.recoverable_errors.truncate(recoverable_errors_count);
            self.cached_token = cached_token;
        }
        result
    }

    fn assert_no_ws_or_comment(&self, left: &Span, right: &Span) -> PResult<()> {
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
}
