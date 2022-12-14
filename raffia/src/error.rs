//! Error management.

use crate::pos::Span;
#[cfg(feature = "serialize")]
use serde::Serialize;
use std::fmt::Display;

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(rename_all = "camelCase"))]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
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
    ExpectContainerConditionAnd,
    ExpectContainerConditionOr,
    ExpectContainerConditionNot,
    ExpectStyleConditionAnd,
    ExpectStyleConditionOr,
    ExpectStyleConditionNot,
    ExpectStyleQuery,
    ExpectSassKeyword(&'static str),
    InvalidAnPlusB,
    ExpectInteger,
    ExpectUnsignedInteger,
    ExpectImportantAnnotation,
    ExpectSassUseNamespace,
    InvalidUnicodeRange,

    TryParseError,
    CSSWideKeywordDisallowed,
    MediaTypeKeywordDisallowed(String),
    UnknownKeyframeSelectorIdent,
    InvalidRatioDenominator,
    ExpectMediaFeatureName,
    ExpectDashedIdent,
    InvalidIdSelectorName,
    ReturnOutsideFunction,
    MaxCodePointExceeded,
    UnicodeRangeStartGreaterThanEnd,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unexpected(expected, actual) => {
                write!(f, "expect token `{expected}`, but `{actual}` received")
            }

            Self::UnknownToken => write!(f, "unknown token"),
            Self::InvalidNumber => write!(f, "invalid number"),
            Self::InvalidEscape => write!(f, "invalid escape"),
            Self::InvalidHash => write!(f, "invalid hash token"),
            Self::ExpectRightBraceForLessVar => write!(f, "`}}` for Less variable is expected"),
            Self::UnexpectedLinebreak => write!(f, "unexpected linebreak"),
            Self::UnexpectedEof => write!(f, "unexpected end of file"),

            Self::UnexpectedWhitespace => write!(f, "unexpected whitespace"),
            Self::ExpectSimpleSelector => write!(f, "simple selector is expected"),
            Self::ExpectTypeSelector => write!(f, "type selector is expected"),
            Self::ExpectIdSelector => write!(f, "ID selector is expected"),
            Self::ExpectWqName => write!(f, "WqName is expected"),
            Self::ExpectAttributeSelectorMatcher => {
                write!(f, "attribute selector matcher is expected")
            }
            Self::ExpectAttributeSelectorValue => write!(f, "attribute selector value is expected"),
            Self::ExpectComponentValue => write!(f, "component value is expected"),
            Self::ExpectSassExpression => write!(f, "Sass expression is expected"),
            Self::ExpectDedentOrEof => write!(f, "dedentation or end of file is expected"),
            Self::ExpectString => write!(f, "string is expected"),
            Self::ExpectUrl => write!(f, "URL is expected"),
            Self::UnexpectedTemplateInCss => write!(f, "template isn't allowed in CSS"),
            Self::ExpectMediaFeatureComparison => write!(f, "media feature comparison is expected"),
            Self::ExpectMediaAnd => write!(f, "media query `and` is expected"),
            Self::ExpectMediaOr => write!(f, "media query `or` is expected"),
            Self::ExpectMediaNot => write!(f, "media query `not` is expected"),
            Self::ExpectContainerConditionAnd => write!(f, "container condition `and` is expected"),
            Self::ExpectContainerConditionOr => write!(f, "container condition `or` is expected"),
            Self::ExpectContainerConditionNot => write!(f, "container condition `not` is expected"),
            Self::ExpectStyleConditionAnd => write!(f, "style condition `and` is expected"),
            Self::ExpectStyleConditionOr => write!(f, "style condition `or` is expected"),
            Self::ExpectStyleConditionNot => write!(f, "style condition `not` is expected"),
            Self::ExpectStyleQuery => write!(f, "style query is expected"),
            Self::ExpectSassKeyword(keyword) => write!(f, "Sass keyword `{keyword}` is expected"),
            Self::InvalidAnPlusB => write!(f, "invalid An+B syntax"),
            Self::ExpectInteger => write!(f, "an integer is expected"),
            Self::ExpectUnsignedInteger => write!(f, "unsigned integer is expected"),
            Self::ExpectImportantAnnotation => write!(f, "`!important` is expected"),
            Self::ExpectSassUseNamespace => {
                write!(f, "`*` or ident for Sass namespace is expected")
            }
            Self::InvalidUnicodeRange => write!(f, "invalid unicode range"),

            Self::TryParseError => unreachable!(),
            Self::CSSWideKeywordDisallowed => {
                write!(f, "using CSS wide keyword as identifier is disallowed")
            }
            Self::MediaTypeKeywordDisallowed(keyword) => {
                write!(f, "keyword `{keyword}` as media type is disallowed")
            }
            Self::UnknownKeyframeSelectorIdent => write!(f, "unknown keyframe selector"),
            Self::InvalidRatioDenominator => write!(f, "ratio denominator is invalid"),
            Self::ExpectMediaFeatureName => write!(f, "media feature name is expected"),
            Self::ExpectDashedIdent => write!(f, "dashed identifier is expected"),
            Self::InvalidIdSelectorName => write!(f, "invalid ID selector name"),
            Self::ReturnOutsideFunction => write!(f, "`@return` is disallowed outside function"),
            Self::MaxCodePointExceeded => {
                write!(f, "unicode range end value exceeds max allowed code point")
            }
            Self::UnicodeRangeStartGreaterThanEnd => {
                write!(f, "unicode range start value can't greater than end value")
            }
        }
    }
}

pub type PResult<T> = Result<T, Error>;
