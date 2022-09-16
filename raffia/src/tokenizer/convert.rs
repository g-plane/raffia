use super::{handle_escape, token};
use crate::{
    ast::{
        Ident, InterpolableIdentStaticPart, InterpolableStrStaticPart, InterpolableUrlStaticPart,
        Number, Str,
    },
    error::{Error, ErrorKind, PResult},
    util::CowStr,
};

impl<'a> From<token::Str<'a>> for Str<'a> {
    fn from(str: token::Str<'a>) -> Self {
        let raw_without_quotes = unsafe { str.raw.get_unchecked(1..str.raw.len() - 1) };
        let value = if str.escaped {
            handle_escape(raw_without_quotes)
        } else {
            CowStr::from(raw_without_quotes)
        };
        Self {
            value,
            raw: str.raw,
            span: str.span,
        }
    }
}

impl<'a> TryFrom<token::Number<'a>> for Number<'a> {
    type Error = Error;

    fn try_from(number: token::Number<'a>) -> PResult<Self> {
        number
            .raw
            .parse()
            .map_err(|_| Error {
                kind: ErrorKind::InvalidNumber,
                span: number.span.clone(),
            })
            .map(|value| Self {
                value,
                raw: number.raw,
                span: number.span,
            })
    }
}

impl TryFrom<token::Number<'_>> for i32 {
    type Error = Error;

    fn try_from(token::Number { raw, span, .. }: token::Number) -> PResult<Self> {
        let value = raw.parse::<f32>().map_err(|_| Error {
            kind: ErrorKind::InvalidNumber,
            span: span.clone(),
        })?;
        if value.fract() == 0.0 {
            // SAFETY: f32 parsed from source text will never be NaN or infinity.
            unsafe { Ok(value.to_int_unchecked()) }
        } else {
            Err(Error {
                kind: ErrorKind::ExpectInteger,
                span,
            })
        }
    }
}

impl<'a> From<token::Ident<'a>> for Ident<'a> {
    fn from(ident: token::Ident<'a>) -> Self {
        Self {
            name: ident.name(),
            raw: ident.raw,
            span: ident.span,
        }
    }
}

impl<'a> From<token::Ident<'a>> for InterpolableIdentStaticPart<'a> {
    fn from(ident: token::Ident<'a>) -> Self {
        Self {
            value: ident.name(),
            raw: ident.raw,
            span: ident.span,
        }
    }
}

impl<'s> From<token::StrTemplate<'s>> for InterpolableStrStaticPart<'s> {
    fn from(token: token::StrTemplate<'s>) -> Self {
        let raw_without_quotes = if token.tail {
            unsafe { token.raw.get_unchecked(0..token.raw.len() - 1) }
        } else if token.head {
            unsafe { token.raw.get_unchecked(1..token.raw.len()) }
        } else {
            token.raw
        };
        let value = if token.escaped {
            handle_escape(raw_without_quotes)
        } else {
            CowStr::from(raw_without_quotes)
        };
        Self {
            value,
            raw: token.raw,
            span: token.span,
        }
    }
}

impl<'s> From<token::UrlTemplate<'s>> for InterpolableUrlStaticPart<'s> {
    fn from(token: token::UrlTemplate<'s>) -> Self {
        let value = if token.escaped {
            handle_escape(token.raw)
        } else {
            CowStr::from(token.raw)
        };
        Self {
            value,
            raw: token.raw,
            span: token.span,
        }
    }
}
