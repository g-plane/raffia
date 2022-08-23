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
