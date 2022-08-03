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
    Eof(Eof),
    Ampersand(Ampersand),
    Asterisk(Asterisk),
    AsteriskEqual(AsteriskEqual),
    AtKeyword(AtKeyword<'a>),
    AtLBraceVar(AtLBraceVar<'a>),
    Bar(Bar),
    BarBar(BarBar),
    BarEqual(BarEqual),
    CaretEqual(CaretEqual),
    Colon(Colon),
    ColonColon(ColonColon),
    Comma(Comma),
    Dedent(Dedent),
    Dimension(Dimension<'a>),
    DollarEqual(DollarEqual),
    DollarVar(DollarVar<'a>),
    Dot(Dot),
    Equal(Equal),
    EqualEqual(EqualEqual),
    ExclamationEqual(ExclamationEqual),
    Function(Function<'a>),
    GreaterThan(GreaterThan),
    GreaterThanEqual(GreaterThanEqual),
    Hash(Hash<'a>),
    HashLBrace(HashLBrace),
    Ident(Ident<'a>),
    Indent(Indent),
    LBrace(LBrace),
    LBracket(LBracket),
    LessThan(LessThan),
    LessThanEqual(LessThanEqual),
    Linebreak(Linebreak),
    LParen(LParen),
    Minus(Minus),
    Number(Number<'a>),
    NumberSign(NumberSign),
    Percent(Percent),
    Percentage(Percentage<'a>),
    Plus(Plus),
    PlusUnderscore(PlusUnderscore),
    RBrace(RBrace),
    RBracket(RBracket),
    RParen(RParen),
    Semicolon(Semicolon),
    Solidus(Solidus),
    Str(Str<'a>),
    Tilde(Tilde),
    TildeEqual(TildeEqual),
    Url(Url<'a>),
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
pub struct AsteriskEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AtKeyword<'a> {
    pub ident: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AtLBraceVar<'a> {
    pub ident: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct BadStr<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
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
pub struct BarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct CaretEqual {
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
pub struct Comma {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Dedent {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Dimension<'a> {
    pub value: Number<'a>,
    pub unit: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct DollarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct DollarVar<'a> {
    pub ident: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Dot {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Eof {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Equal {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct EqualEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct ExclamationEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct GreaterThan {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct GreaterThanEqual {
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
pub struct HashLBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Ident<'a> {
    pub name: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Indent {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LBracket {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessThan {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LessThanEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Linebreak {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LParen {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Minus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Number<'a> {
    pub value: f64,
    pub raw: &'a str,
    pub span: Span,
}

/// U+0023 `#`
#[derive(Clone, Debug, Spanned)]
pub struct NumberSign {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Percent {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Percentage<'a> {
    pub value: Number<'a>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Plus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct PlusUnderscore {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct RBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct RBracket {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct RParen {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Semicolon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Solidus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Str<'a> {
    pub value: Cow<'a, str>,
    pub raw: &'a str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Tilde {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct TildeEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Url<'a> {
    pub value: UrlValue<'a>,
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
