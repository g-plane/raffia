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
    Unexpected(&'static str),

    UnknownToken,
    InvalidNumber,
    InvalidEscape,
    InvalidHash,
    ExpectRightParenForURL,
    ExpectRightBraceForLessVar,
    UnexpectedEof,

    UnexpectedWhitespace,
    ExpectSimpleSelector,
    InvalidIdSelectorName,
    ExpectTypeSelector,
    ExpectIdSelector,
    ExpectWqName,
    ExpectAttributeSelectorMatcher,
    ExpectAttributeSelectorValue,
    ExpectComponentValue,
    ExpectSassExpression,
}

pub type PResult<T> = Result<T, Error>;
