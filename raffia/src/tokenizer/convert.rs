use super::token;
use crate::{
    ast::{Ident, InterpolableIdentStaticPart, Number, Str},
    error::{Error, ErrorKind, PResult},
};

impl<'a> From<token::Str<'a>> for Str<'a> {
    fn from(str: token::Str<'a>) -> Self {
        Self {
            value: str.value,
            raw: str.raw,
            span: str.span,
        }
    }
}

impl<'a> From<token::Number<'a>> for Number<'a> {
    fn from(number: token::Number<'a>) -> Self {
        Self {
            value: number.value,
            raw: number.raw,
            span: number.span,
        }
    }
}

impl TryFrom<token::Number<'_>> for i32 {
    type Error = Error;

    fn try_from(token::Number { value, span, .. }: token::Number) -> PResult<Self> {
        if value.fract() == 0.0 {
            // SAFETY: f64 parsed from source text will never be NaN or infinity.
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
            name: ident.name,
            raw: ident.raw,
            span: ident.span,
        }
    }
}

impl<'a> From<token::Ident<'a>> for InterpolableIdentStaticPart<'a> {
    fn from(ident: token::Ident<'a>) -> Self {
        Self {
            value: ident.name,
            raw: ident.raw,
            span: ident.span,
        }
    }
}
