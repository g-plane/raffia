use crate::{
    ast::{
        Ident, InterpolableIdentStaticPart, InterpolableStrStaticPart, InterpolableUrlStaticPart,
        Number,
    },
    error::{Error, ErrorKind, PResult},
    tokenizer::token,
    util::{handle_escape, CowStr},
    Span,
};

impl<'s> Ident<'s> {
    pub(super) fn from_token(token: token::Ident<'s>, span: Span) -> Self {
        Ident {
            name: token.name(),
            raw: token.raw,
            span,
        }
    }
}

impl<'s> InterpolableIdentStaticPart<'s> {
    pub(super) fn from_token(token: token::Ident<'s>, span: Span) -> Self {
        InterpolableIdentStaticPart {
            value: token.name(),
            raw: token.raw,
            span,
        }
    }
}

impl<'s> Number<'s> {
    pub(super) fn try_from_token(token: token::Number<'s>, span: Span) -> PResult<Self> {
        token
            .raw
            .parse()
            .map_err(|_| Error {
                kind: ErrorKind::InvalidNumber,
                span: span.clone(),
            })
            .map(|value| Self {
                value,
                raw: token.raw,
                span,
            })
    }
}

impl<'s> InterpolableStrStaticPart<'s> {
    pub(super) fn from_token(token: token::StrTemplate<'s>, span: Span) -> Self {
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
            span,
        }
    }
}

impl<'s> InterpolableUrlStaticPart<'s> {
    pub(super) fn from_token(token: token::UrlTemplate<'s>, span: Span) -> Self {
        let value = if token.escaped {
            handle_escape(token.raw)
        } else {
            CowStr::from(token.raw)
        };
        Self {
            value,
            raw: token.raw,
            span,
        }
    }
}
