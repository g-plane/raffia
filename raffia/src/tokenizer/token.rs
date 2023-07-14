//! All supported tokens, and with comments.

use crate::pos::Span;
use raffia_macro::{EnumAsIs, SpanIgnoredEq, Spanned};
#[cfg(feature = "serialize")]
use serde::Serialize;

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum Comment<'s> {
    Block(BlockComment<'s>),
    Line(LineComment<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct BlockComment<'s> {
    pub content: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned, PartialEq, SpanIgnoredEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct LineComment<'s> {
    pub content: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, EnumAsIs)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum Token<'s> {
    Eof(Eof),
    Ampersand(Ampersand),
    Asterisk(Asterisk),
    AsteriskEqual(AsteriskEqual),
    At(At),
    AtKeyword(AtKeyword<'s>),
    AtLBraceVar(AtLBraceVar<'s>),
    Backtick(Backtick),
    BadStr(BadStr<'s>),
    Bar(Bar),
    BarBar(BarBar),
    BarEqual(BarEqual),
    CaretEqual(CaretEqual),
    Cdc(Cdc),
    Cdo(Cdo),
    Colon(Colon),
    ColonColon(ColonColon),
    Comma(Comma),
    Dedent(Dedent),
    Dimension(Dimension<'s>),
    DollarEqual(DollarEqual),
    DollarVar(DollarVar<'s>),
    Dot(Dot),
    DotDotDot(DotDotDot),
    Equal(Equal),
    EqualEqual(EqualEqual),
    Exclamation(Exclamation),
    ExclamationEqual(ExclamationEqual),
    GreaterThan(GreaterThan),
    GreaterThanEqual(GreaterThanEqual),
    Hash(Hash<'s>),
    HashLBrace(HashLBrace),
    Ident(Ident<'s>),
    Indent(Indent),
    LBrace(LBrace),
    LBracket(LBracket),
    LessThan(LessThan),
    LessThanEqual(LessThanEqual),
    Linebreak(Linebreak),
    LParen(LParen),
    Minus(Minus),
    Number(Number<'s>),
    NumberSign(NumberSign),
    Percent(Percent),
    Percentage(Percentage<'s>),
    Plus(Plus),
    PlusUnderscore(PlusUnderscore),
    Question(Question),
    RBrace(RBrace),
    RBracket(RBracket),
    RParen(RParen),
    Semicolon(Semicolon),
    Solidus(Solidus),
    Str(Str<'s>),
    StrTemplate(StrTemplate<'s>),
    Tilde(Tilde),
    TildeEqual(TildeEqual),
    UrlRaw(UrlRaw<'s>),
    UrlTemplate(UrlTemplate<'s>),
}

#[derive(Clone, Debug, Spanned, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type", rename_all = "camelCase"))]
pub struct TokenWithSpan<'s> {
    pub token: Token<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Ampersand {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Asterisk {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct AsteriskEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct At {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct AtKeyword<'s> {
    pub ident: Ident<'s>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct AtLBraceVar<'s> {
    pub ident: Ident<'s>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Backtick {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct BadStr<'s> {
    pub raw: &'s str,
    pub escaped: bool,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Bar {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct BarBar {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct BarEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct CaretEqual {}

/// `-->`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Cdc {}

/// `<!--`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Cdo {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Colon {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct ColonColon {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Comma {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Dedent {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Dimension<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct DollarEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct DollarVar<'s> {
    pub ident: Ident<'s>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Dot {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct DotDotDot {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Eof {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Equal {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct EqualEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Exclamation {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct ExclamationEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct GreaterThan {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct GreaterThanEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Hash<'s> {
    /// raw string without beginning `#` char
    pub raw: &'s str,
    pub escaped: bool,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct HashLBrace {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Ident<'s> {
    pub escaped: bool,
    pub raw: &'s str,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Indent {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct LBrace {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct LBracket {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct LessThan {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct LessThanEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Linebreak {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct LParen {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Minus {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Number<'s> {
    pub raw: &'s str,
}

/// U+0023 `#`
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct NumberSign {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Percent {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Percentage<'s> {
    pub value: Number<'s>,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Plus {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct PlusUnderscore {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Question {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct RBrace {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct RBracket {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct RParen {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Semicolon {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Solidus {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Str<'s> {
    pub raw: &'s str,
    pub escaped: bool,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct StrTemplate<'s> {
    pub raw: &'s str,
    pub escaped: bool,
    pub head: bool,
    pub tail: bool,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct Tilde {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct TildeEqual {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct UrlRaw<'s> {
    pub raw: &'s str,
    pub escaped: bool,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind", rename_all = "camelCase"))]
pub struct UrlTemplate<'s> {
    pub raw: &'s str,
    pub escaped: bool,
    pub tail: bool,
}
