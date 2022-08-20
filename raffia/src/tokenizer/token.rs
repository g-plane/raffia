use crate::pos::Span;
use beef::Cow;
use raffia_derive::Spanned;
#[cfg(feature = "serialize")]
use serde::Serialize;

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(untagged))]
pub enum Comment<'s> {
    Block(BlockComment<'s>),
    Line(LineComment<'s>),
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type"))]
pub struct BlockComment<'s> {
    pub content: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "type"))]
pub struct LineComment<'s> {
    pub content: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub enum Token<'s> {
    Eof(Eof),
    Ampersand(Ampersand),
    Asterisk(Asterisk),
    AsteriskEqual(AsteriskEqual),
    AtKeyword(AtKeyword<'s>),
    AtLBraceVar(AtLBraceVar<'s>),
    Bar(Bar),
    BarBar(BarBar),
    BarEqual(BarEqual),
    CaretEqual(CaretEqual),
    Colon(Colon),
    ColonColon(ColonColon),
    Comma(Comma),
    Dedent(Dedent),
    Dimension(Dimension<'s>),
    DollarEqual(DollarEqual),
    DollarVar(DollarVar<'s>),
    Dot(Dot),
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
    RBrace(RBrace),
    RBracket(RBracket),
    RParen(RParen),
    Semicolon(Semicolon),
    Solidus(Solidus),
    Str(Str<'s>),
    StrTemplate(StrTemplate<'s>),
    Tilde(Tilde),
    TildeEqual(TildeEqual),
    UrlPrefix(UrlPrefix<'s>),
    UrlRaw(UrlRaw<'s>),
    UrlTemplate(UrlTemplate<'s>),
}

impl Token<'_> {
    pub(crate) fn symbol(&self) -> &'static str {
        use Token::*;
        match self {
            Eof(..) => "<eof>",
            Ampersand(..) => "&",
            Asterisk(..) => "*",
            AsteriskEqual(..) => "*=",
            AtKeyword(..) => "<at-keyword>",
            AtLBraceVar(..) => "@{",
            Bar(..) => "|",
            BarBar(..) => "||",
            BarEqual(..) => "|=",
            CaretEqual(..) => "^=",
            Colon(..) => ":",
            ColonColon(..) => "::",
            Comma(..) => ",",
            Dedent(..) => "<dedent>",
            Dimension(..) => "<dimension>",
            DollarEqual(..) => "$=",
            DollarVar(..) => "$var",
            Dot(..) => ".",
            Equal(..) => "=",
            EqualEqual(..) => "==",
            Exclamation(..) => "!",
            ExclamationEqual(..) => "!=",
            GreaterThan(..) => ">",
            GreaterThanEqual(..) => ">=",
            Hash(..) => "<hash>",
            HashLBrace(..) => "#{",
            Ident(..) => "<ident>",
            Indent(..) => "<indent>",
            LBrace(..) => "{",
            LBracket(..) => "[",
            LessThan(..) => "<",
            LessThanEqual(..) => "<=",
            Linebreak(..) => "<linebreak>",
            LParen(..) => "(",
            Minus(..) => "-",
            Number(..) => "<number>",
            NumberSign(..) => "#",
            Percent(..) => "%",
            Percentage(..) => "<percentage>",
            Plus(..) => "+",
            PlusUnderscore(..) => "+_",
            RBrace(..) => "}",
            RBracket(..) => "]",
            RParen(..) => ")",
            Semicolon(..) => ";",
            Solidus(..) => "/",
            Str(..) => "<string>",
            StrTemplate(..) => "<string template>",
            Tilde(..) => "~",
            TildeEqual(..) => "~=",
            UrlPrefix(..) => "url(",
            UrlRaw(..) => "<url>",
            UrlTemplate(..) => "<url template>",
        }
    }
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Ampersand {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Asterisk {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct AsteriskEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct AtKeyword<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct AtLBraceVar<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Bar {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct BarBar {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct BarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct CaretEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Colon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct ColonColon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Comma {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Dedent {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Dimension<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct DollarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct DollarVar<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Dot {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Eof {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Equal {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct EqualEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Exclamation {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct ExclamationEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct GreaterThan {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct GreaterThanEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Hash<'s> {
    pub value: Cow<'s, str>,
    /// raw string with beginning `#` char
    pub raw: &'s str,
    /// raw string without beginning `#` char
    pub raw_without_hash: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct HashLBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Ident<'s> {
    pub name: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Indent {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct LBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct LBracket {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct LessThan {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct LessThanEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Linebreak {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct LParen {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Minus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Number<'s> {
    pub value: f64,
    pub raw: &'s str,
    pub span: Span,
}

/// U+0023 `#`
#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct NumberSign {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Percent {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Percentage<'s> {
    pub value: Number<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Plus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct PlusUnderscore {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct RBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct RBracket {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct RParen {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Semicolon {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Solidus {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Str<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct StrTemplate<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub tail: bool,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct Tilde {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct TildeEqual {
    pub span: Span,
}

/// `url(` only
#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct UrlPrefix<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct UrlRaw<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(tag = "kind"))]
pub struct UrlTemplate<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub tail: bool,
    pub span: Span,
}
