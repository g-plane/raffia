use crate::pos::Span;
use std::{error, fmt::Display};

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

impl error::Error for Error {}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    Unexpected,

    InvalidNumber,
    InvalidEscape,
    InvalidHash,
    ExpectRightParenForURL,
    UnexpectedEof,

    UnexpectedWhitespace,
    ExpectSimpleSelector,
    InvalidIdSelectorName,
    ExpectTypeSelector,
}

pub type PResult<T> = Result<T, Error>;
