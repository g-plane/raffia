#[doc(hidden)]
#[macro_export]
macro_rules! bump {
    ($parser:expr) => {{
        match $parser.cached_token.take() {
            Some(token_with_span) => token_with_span,
            None => {
                let tokenizer = &mut $parser.tokenizer;
                tokenizer.bump()?
            }
        }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! expect {
    ($parser:expr, $variant:ident) => {{
        use $crate::{
            bump,
            error::{Error, ErrorKind},
            tokenizer::{Token, TokenSymbol, TokenWithSpan},
        };
        match bump!($parser) {
            TokenWithSpan {
                token: Token::$variant(token),
                span,
            } => (token, span),
            TokenWithSpan { token, span } => {
                return Err(Error {
                    kind: ErrorKind::Unexpected(
                        $crate::tokenizer::token::$variant::symbol(),
                        token.symbol(),
                    ),
                    span,
                });
            }
        }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! expect_without_ws_or_comments {
    ($parser:expr, Ident) => {{
        use $crate::{
            error::{Error, ErrorKind},
            tokenizer::TokenSymbol,
        };
        debug_assert!($parser.cached_token.is_none());
        let tokenizer = &mut $parser.tokenizer;
        if tokenizer.is_start_of_ident() {
            tokenizer.scan_ident_sequence()?
        } else {
            let token_with_span = tokenizer.bump_without_ws_or_comments()?;
            return Err(Error {
                kind: ErrorKind::Unexpected(
                    $crate::tokenizer::token::Ident::symbol(),
                    token_with_span.token.symbol(),
                ),
                span: token_with_span.span,
            });
        }
    }};
    ($parser:expr, $variant:ident) => {{
        use $crate::{
            error::{Error, ErrorKind},
            tokenizer::{Token, TokenSymbol, TokenWithSpan},
        };
        debug_assert!($parser.cached_token.is_none());
        let tokenizer = &mut $parser.tokenizer;
        let token_with_span = tokenizer.bump_without_ws_or_comments()?;
        match token_with_span {
            TokenWithSpan {
                token: Token::$variant(token),
                span,
            } => (token, span),
            TokenWithSpan { token, span } => {
                return Err(Error {
                    kind: ErrorKind::Unexpected(
                        $crate::tokenizer::token::$variant::symbol(),
                        token.symbol(),
                    ),
                    span,
                });
            }
        }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use $crate::{
            bump,
            tokenizer::{Token, TokenWithSpan},
        };
        let token_with_span = bump!($parser);
        match token_with_span {
            TokenWithSpan {
                token: Token::$variant(token),
                span,
            } => Some((token, span)),
            value => {
                $parser.cached_token = Some(value);
                None
            }
        }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! peek {
    ($parser:expr) => {{
        match &$parser.cached_token {
            Some(token_with_span) => token_with_span,
            None => {
                let tokenizer = &mut $parser.tokenizer;
                let token = tokenizer.bump()?;
                $parser.cached_token.insert(token)
            }
        }
    }};
}
