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
    Unexpected(
        /* expected */ &'static str,
        /* actual */ &'static str,
    ),

    UnknownToken,
    InvalidNumber,
    InvalidEscape,
    InvalidHash,
    ExpectRightBraceForLessVar,
    UnexpectedLinebreak,
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
    ExpectDedentOrEof,
    ExpectString,
    ExpectUrl,
    UnexpectedTemplateInCss,
    ExpectMediaFeatureComparison,
    ExpectMediaAnd,
    ExpectMediaOr,
    ExpectMediaNot,
    ExpectSassKeyword(&'static str),

    CSSWideKeywordDisallowed,
    UnknownKeyframeSelectorIdent,
    InvalidRatioDenominator,
    ExpectMediaFeatureName,
    InvalidExtensionName,
    ExpectDashedIdent,
}

pub type PResult<T> = Result<T, Error>;
