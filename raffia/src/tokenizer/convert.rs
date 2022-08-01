use super::token;
use crate::ast::{Ident, InterpolableIdentStaticPart, Number, Str};

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
