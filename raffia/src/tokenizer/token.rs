use crate::pos::Span;
use raffia_derive::Spanned;
use std::borrow::Cow;

#[derive(Clone, Debug, Spanned)]
pub enum Comment<'s> {
    Block(BlockComment<'s>),
    Line(LineComment<'s>),
}

#[derive(Clone, Debug, Spanned)]
pub struct BlockComment<'s> {
    pub content: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct LineComment<'s> {
    pub content: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
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
pub struct AtKeyword<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct AtLBraceVar<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct BadStr<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
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
pub struct Dimension<'s> {
    pub value: Number<'s>,
    pub unit: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct DollarEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct DollarVar<'s> {
    pub ident: Ident<'s>,
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
pub struct GreaterThan {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct GreaterThanEqual {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Hash<'s> {
    pub value: Cow<'s, str>,
    /// raw string with beginning `#` char
    pub raw: &'s str,
    /// raw string without beginning `#` char
    pub raw_without_hash: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct HashLBrace {
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct Ident<'s> {
    pub name: Cow<'s, str>,
    pub raw: &'s str,
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
pub struct Number<'s> {
    pub value: f64,
    pub raw: &'s str,
    pub signed: bool,
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
pub struct Percentage<'s> {
    pub value: Number<'s>,
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
pub struct Str<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct StrTemplate<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub tail: bool,
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

/// `url(` only
#[derive(Clone, Debug, Spanned)]
pub struct UrlPrefix<'s> {
    pub ident: Ident<'s>,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct UrlRaw<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub span: Span,
}

#[derive(Clone, Debug, Spanned)]
pub struct UrlTemplate<'s> {
    pub value: Cow<'s, str>,
    pub raw: &'s str,
    pub tail: bool,
    pub span: Span,
}
