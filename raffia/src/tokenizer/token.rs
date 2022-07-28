use crate::pos::Span;
use raffia_derive::Spanned;
use std::borrow::Cow;

#[derive(Clone, Debug, Spanned)]
pub enum Comment<'a> {
    Block(BlockComment<'a>),
    Line(LineComment<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct BlockComment<'a> {
    pub content: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LineComment<'a> {
    pub content: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum Token<'a> {
    Unknown,
    Eof,
    Ident(Ident<'a>),
    Number(Number<'a>),
    Dimension(Dimension<'a>),
    Percentage(Percentage<'a>),
    Str(Str<'a>),
    Url(Url<'a>),
    Hash(Hash<'a>),
    DollarVar(DollarVar<'a>),
    AtKeyword(AtKeyword<'a>),
    Function(Function<'a>),
    Colon(Colon),
    ColonColon(ColonColon),
    LParen(LParen),
    RParen(RParen),
    LBracket(LBracket),
    RBracket(RBracket),
    LBrace(LBrace),
    RBrace(RBrace),
    Solidus(Solidus),
    Comma(Comma),
    Semicolon(Semicolon),
    Dot(Dot),
    Plus(Plus),
    GreaterThan(GreaterThan),
    Tilde(Tilde),
    Bar(Bar),
    BarBar(BarBar),
    Ampersand(Ampersand),
    Asterisk(Asterisk),
    Equal(Equal),
    TildeEqual(TildeEqual),
    BarEqual(BarEqual),
    CaretEqual(CaretEqual),
    DollarEqual(DollarEqual),
    AsteriskEqual(AsteriskEqual),
}

#[derive(Clone, Debug, Spanned)]
pub struct Ident<'a> {
    pub name: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Number<'a> {
    pub value: f64,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Dimension<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Percentage<'a> {
    pub value: Number<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Str<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct BadStr<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct UrlRaw<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub enum UrlValue<'a> {
    Str(Str<'a>),
    Raw(UrlRaw<'a>),
}

#[derive(Clone, Debug, Spanned)]
pub struct Url<'a> {
    pub value: UrlValue<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Hash<'a> {
    pub value: Cow<'a, str>,
    /// raw string with beginning `#` char
    pub raw: &'a str,
    /// raw string without beginning `#` char
    pub raw_without_hash: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct DollarVar<'a> {
    pub ident: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AtKeyword<'a> {
    pub ident: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Colon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct ColonColon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LParen {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct RParen {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LBracket {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct RBracket {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct RBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Solidus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Comma {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Semicolon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Dot {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Plus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct GreaterThan {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Tilde {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Bar {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct BarBar {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Ampersand {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Asterisk {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Equal {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct TildeEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct BarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct CaretEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct DollarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AsteriskEqual {
    pub span: Span,
}
