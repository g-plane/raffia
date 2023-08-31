use super::token::*;

pub(crate) trait TokenSymbol {
    fn symbol() -> &'static str;
}

impl TokenSymbol for Eof {
    #[inline]
    fn symbol() -> &'static str {
        "<eof>"
    }
}

impl TokenSymbol for Ampersand {
    #[inline]
    fn symbol() -> &'static str {
        "&"
    }
}

impl TokenSymbol for Asterisk {
    #[inline]
    fn symbol() -> &'static str {
        "*"
    }
}

impl TokenSymbol for AsteriskEqual {
    #[inline]
    fn symbol() -> &'static str {
        "*="
    }
}

impl TokenSymbol for At {
    #[inline]
    fn symbol() -> &'static str {
        "@"
    }
}

impl TokenSymbol for AtKeyword<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<at-keyword>"
    }
}

impl TokenSymbol for AtLBraceVar<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "@{"
    }
}

impl TokenSymbol for BacktickCode<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<backtick code>"
    }
}

impl TokenSymbol for Bar {
    #[inline]
    fn symbol() -> &'static str {
        "|"
    }
}

impl TokenSymbol for BarBar {
    #[inline]
    fn symbol() -> &'static str {
        "||"
    }
}

impl TokenSymbol for BarEqual {
    #[inline]
    fn symbol() -> &'static str {
        "|="
    }
}

impl TokenSymbol for CaretEqual {
    #[inline]
    fn symbol() -> &'static str {
        "^="
    }
}

impl TokenSymbol for Cdc {
    #[inline]
    fn symbol() -> &'static str {
        "<CDC>"
    }
}

impl TokenSymbol for Cdo {
    #[inline]
    fn symbol() -> &'static str {
        "<CDO>"
    }
}

impl TokenSymbol for Colon {
    #[inline]
    fn symbol() -> &'static str {
        ":"
    }
}

impl TokenSymbol for ColonColon {
    #[inline]
    fn symbol() -> &'static str {
        "::"
    }
}

impl TokenSymbol for Comma {
    #[inline]
    fn symbol() -> &'static str {
        ","
    }
}

impl TokenSymbol for Dedent {
    #[inline]
    fn symbol() -> &'static str {
        "<dedent>"
    }
}

impl TokenSymbol for Dimension<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<dimension>"
    }
}

impl TokenSymbol for DollarEqual {
    #[inline]
    fn symbol() -> &'static str {
        "$="
    }
}

impl TokenSymbol for DollarLBraceVar<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "${"
    }
}

impl TokenSymbol for DollarVar<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "$var"
    }
}

impl TokenSymbol for Dot {
    #[inline]
    fn symbol() -> &'static str {
        "."
    }
}

impl TokenSymbol for DotDotDot {
    #[inline]
    fn symbol() -> &'static str {
        "..."
    }
}

impl TokenSymbol for Equal {
    #[inline]
    fn symbol() -> &'static str {
        "="
    }
}

impl TokenSymbol for EqualEqual {
    #[inline]
    fn symbol() -> &'static str {
        "=="
    }
}

impl TokenSymbol for Exclamation {
    #[inline]
    fn symbol() -> &'static str {
        "!"
    }
}

impl TokenSymbol for ExclamationEqual {
    #[inline]
    fn symbol() -> &'static str {
        "!="
    }
}

impl TokenSymbol for GreaterThan {
    #[inline]
    fn symbol() -> &'static str {
        ">"
    }
}

impl TokenSymbol for GreaterThanEqual {
    #[inline]
    fn symbol() -> &'static str {
        ">="
    }
}

impl TokenSymbol for Hash<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<hash>"
    }
}

impl TokenSymbol for HashLBrace {
    #[inline]
    fn symbol() -> &'static str {
        "#{"
    }
}

impl TokenSymbol for Ident<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<ident>"
    }
}

impl TokenSymbol for Indent {
    #[inline]
    fn symbol() -> &'static str {
        "<indent>"
    }
}

impl TokenSymbol for LBrace {
    #[inline]
    fn symbol() -> &'static str {
        "{"
    }
}

impl TokenSymbol for LBracket {
    #[inline]
    fn symbol() -> &'static str {
        "["
    }
}

impl TokenSymbol for LessThan {
    #[inline]
    fn symbol() -> &'static str {
        "<"
    }
}

impl TokenSymbol for LessThanEqual {
    #[inline]
    fn symbol() -> &'static str {
        "<="
    }
}

impl TokenSymbol for Linebreak {
    #[inline]
    fn symbol() -> &'static str {
        "<linebreak>"
    }
}

impl TokenSymbol for LParen {
    #[inline]
    fn symbol() -> &'static str {
        "("
    }
}

impl TokenSymbol for Minus {
    #[inline]
    fn symbol() -> &'static str {
        "-"
    }
}

impl TokenSymbol for Number<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<number>"
    }
}

impl TokenSymbol for NumberSign {
    #[inline]
    fn symbol() -> &'static str {
        "#"
    }
}

impl TokenSymbol for Percent {
    #[inline]
    fn symbol() -> &'static str {
        "%"
    }
}

impl TokenSymbol for Percentage<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<percentage>"
    }
}

impl TokenSymbol for Plus {
    #[inline]
    fn symbol() -> &'static str {
        "+"
    }
}

impl TokenSymbol for PlusUnderscore {
    #[inline]
    fn symbol() -> &'static str {
        "+_"
    }
}

impl TokenSymbol for Question {
    #[inline]
    fn symbol() -> &'static str {
        "?"
    }
}

impl TokenSymbol for RBrace {
    #[inline]
    fn symbol() -> &'static str {
        "}"
    }
}

impl TokenSymbol for RBracket {
    #[inline]
    fn symbol() -> &'static str {
        "]"
    }
}

impl TokenSymbol for RParen {
    #[inline]
    fn symbol() -> &'static str {
        ")"
    }
}

impl TokenSymbol for Semicolon {
    #[inline]
    fn symbol() -> &'static str {
        ";"
    }
}

impl TokenSymbol for Solidus {
    #[inline]
    fn symbol() -> &'static str {
        "/"
    }
}

impl TokenSymbol for Str<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<string>"
    }
}

impl TokenSymbol for StrTemplate<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<string template>"
    }
}

impl TokenSymbol for Tilde {
    #[inline]
    fn symbol() -> &'static str {
        "~"
    }
}

impl TokenSymbol for TildeEqual {
    #[inline]
    fn symbol() -> &'static str {
        "~="
    }
}

impl TokenSymbol for UrlRaw<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<url>"
    }
}

impl TokenSymbol for UrlTemplate<'_> {
    #[inline]
    fn symbol() -> &'static str {
        "<url template>"
    }
}

impl Token<'_> {
    pub(crate) fn symbol(&self) -> &'static str {
        use Token::*;
        match self {
            Eof(..) => "<eof>",
            Ampersand(..) => "&",
            Asterisk(..) => "*",
            AsteriskEqual(..) => "*=",
            At(..) => "@",
            AtKeyword(..) => "<at-keyword>",
            AtLBraceVar(..) => "@{",
            BacktickCode(..) => "<backtick code>",
            Bar(..) => "|",
            BarBar(..) => "||",
            BarEqual(..) => "|=",
            CaretEqual(..) => "^=",
            Cdc(..) => "<CDC>",
            Cdo(..) => "<CDO>",
            Colon(..) => ":",
            ColonColon(..) => "::",
            Comma(..) => ",",
            Dedent(..) => "<dedent>",
            Dimension(..) => "<dimension>",
            DollarEqual(..) => "$=",
            DollarLBraceVar(..) => "${",
            DollarVar(..) => "$var",
            Dot(..) => ".",
            DotDotDot(..) => "...",
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
            Question(..) => "?",
            RBrace(..) => "}",
            RBracket(..) => "]",
            RParen(..) => ")",
            Semicolon(..) => ";",
            Solidus(..) => "/",
            Str(..) => "<string>",
            StrTemplate(..) => "<string template>",
            Tilde(..) => "~",
            TildeEqual(..) => "~=",
            UrlRaw(..) => "<url>",
            UrlTemplate(..) => "<url template>",
        }
    }
}
