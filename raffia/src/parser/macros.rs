#[doc(hidden)]
#[macro_export]
macro_rules! bump {
    ($parser:expr) => {{
        match $parser.cached_token.take() {
            Some(token) => token,
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
            error::{Error, ErrorKind},
            tokenizer::{Token, TokenSymbol},
        };
        let token = match $parser.cached_token.take() {
            Some(token) => token,
            None => {
                let tokenizer = &mut $parser.tokenizer;
                tokenizer.bump()?
            }
        };
        match token {
            Token::$variant(token) => token,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected(
                        $crate::tokenizer::token::$variant::symbol(),
                        token.symbol(),
                    ),
                    span: token.span().clone(),
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
            let token = tokenizer.bump_without_ws_or_comments()?;
            return Err(Error {
                kind: ErrorKind::Unexpected(
                    $crate::tokenizer::token::Ident::symbol(),
                    token.symbol(),
                ),
                span: token.span().clone(),
            });
        }
    }};
    ($parser:expr, $variant:ident) => {{
        use $crate::{
            error::{Error, ErrorKind},
            tokenizer::{Token, TokenSymbol},
        };
        debug_assert!($parser.cached_token.is_none());
        let tokenizer = &mut $parser.tokenizer;
        match tokenizer.bump_without_ws_or_comments()? {
            Token::$variant(token) => token,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected(
                        $crate::tokenizer::token::$variant::symbol(),
                        token.symbol(),
                    ),
                    span: token.span().clone(),
                });
            }
        }
    }};
}

#[doc(hidden)]
#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use $crate::{bump, tokenizer::Token};
        match bump!($parser) {
            Token::$variant(token) => Some(token),
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
            Some(token) => token,
            None => {
                let tokenizer = &mut $parser.tokenizer;
                let token = tokenizer.bump()?;
                $parser.cached_token = Some(token);
                // SAFETY: We've written `Some(..)` value to `cached_token`, so it won't be `None`.
                unsafe { $parser.cached_token.as_ref().unwrap_unchecked() }
            }
        }
    }};
}
