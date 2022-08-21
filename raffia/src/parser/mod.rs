use self::state::ParserState;
use crate::{
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    pos::Span,
    tokenizer::Tokenizer,
};
pub use builder::ParserBuilder;

mod at_rule;
mod builder;
mod less;
mod sass;
mod selector;
mod state;
mod stmt;
mod value;

#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use $crate::tokenizer::Token;
        let tokenizer = &mut $parser.tokenizer;
        if let Token::$variant(token) = tokenizer.peek()? {
            let _ = tokenizer.bump();
            Some(token)
        } else {
            None
        }
    }};
}

#[macro_export]
macro_rules! expect {
    ($parser:expr, $variant:ident) => {{
        use $crate::{
            error::{Error, ErrorKind},
            tokenizer::Token,
        };
        let tokenizer = &mut $parser.tokenizer;
        match tokenizer.bump()? {
            Token::$variant(token) => token,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected(stringify!($variant), token.symbol()),
                    span: token.span().clone(),
                });
            }
        }
    }};
}

pub trait Parse<'cmt, 's: 'cmt>: Sized {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self>;
}

pub struct Parser<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    tokenizer: Tokenizer<'cmt, 's>,
    state: ParserState,
    recoverable_errors: Vec<Error>,
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub fn new(source: &'s str, syntax: Syntax) -> Self {
        Parser {
            source,
            syntax: syntax.clone(),
            tokenizer: Tokenizer::new(source, syntax, None),
            state: Default::default(),
            recoverable_errors: vec![],
        }
    }

    pub fn parse<T>(&mut self) -> PResult<T>
    where
        T: Parse<'cmt, 's>,
    {
        T::parse(self)
    }

    #[inline]
    pub fn recoverable_errors(&self) -> &[Error] {
        &self.recoverable_errors
    }

    fn try_parse<R, F: Fn(&mut Self) -> PResult<R>>(&mut self, f: F) -> PResult<R> {
        let tokenizer_state = self.tokenizer.state.clone();
        let comments_count = self
            .tokenizer
            .comments
            .as_ref()
            .map(|comments| comments.len());
        let recoverable_errors_count = self.recoverable_errors.len();
        let result = f(self);
        if result.is_err() {
            self.tokenizer.state = tokenizer_state;
            if let Some((comments, count)) = self.tokenizer.comments.as_mut().zip(comments_count) {
                comments.truncate(count);
            }
            self.recoverable_errors.truncate(recoverable_errors_count);
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
