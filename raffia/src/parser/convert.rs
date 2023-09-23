use crate::{
    ast::{
        Dimension, DimensionKind, Ident, InterpolableIdentStaticPart, InterpolableStrStaticPart,
        InterpolableUrlStaticPart, Number, Str,
    },
    error::{Error, ErrorKind, PResult},
    tokenizer::token,
    util, Span,
};
use std::borrow::Cow;

impl<'s> TryFrom<(token::Dimension<'s>, Span)> for Dimension<'s> {
    type Error = Error;

    fn try_from((token, span): (token::Dimension<'s>, Span)) -> PResult<Self> {
        let value_span = Span {
            start: span.start,
            end: span.start + token.value.raw.len(),
        };
        let unit_span = Span {
            start: span.start + token.value.raw.len(),
            end: span.end,
        };

        let value = (token.value, value_span).try_into()?;
        let unit = Ident::from((token.unit, unit_span));
        let unit_name = &unit.name;
        let kind = if unit_name.eq_ignore_ascii_case("px")
            || unit_name.eq_ignore_ascii_case("em")
            || unit_name.eq_ignore_ascii_case("rem")
            || unit_name.eq_ignore_ascii_case("ex")
            || unit_name.eq_ignore_ascii_case("rex")
            || unit_name.eq_ignore_ascii_case("cap")
            || unit_name.eq_ignore_ascii_case("rcap")
            || unit_name.eq_ignore_ascii_case("ch")
            || unit_name.eq_ignore_ascii_case("rch")
            || unit_name.eq_ignore_ascii_case("ic")
            || unit_name.eq_ignore_ascii_case("ric")
            || unit_name.eq_ignore_ascii_case("lh")
            || unit_name.eq_ignore_ascii_case("rlh")
            || unit_name.eq_ignore_ascii_case("vw")
            || unit_name.eq_ignore_ascii_case("vh")
            || unit_name.eq_ignore_ascii_case("vi")
            || unit_name.eq_ignore_ascii_case("vb")
            || unit_name.eq_ignore_ascii_case("vmin")
            || unit_name.eq_ignore_ascii_case("vmax")
            || unit_name.eq_ignore_ascii_case("lvw")
            || unit_name.eq_ignore_ascii_case("lvh")
            || unit_name.eq_ignore_ascii_case("lvi")
            || unit_name.eq_ignore_ascii_case("lvb")
            || unit_name.eq_ignore_ascii_case("lvmin")
            || unit_name.eq_ignore_ascii_case("lvmax")
            || unit_name.eq_ignore_ascii_case("svw")
            || unit_name.eq_ignore_ascii_case("svh")
            || unit_name.eq_ignore_ascii_case("svi")
            || unit_name.eq_ignore_ascii_case("svb")
            || unit_name.eq_ignore_ascii_case("vmin")
            || unit_name.eq_ignore_ascii_case("vmax")
            || unit_name.eq_ignore_ascii_case("dvw")
            || unit_name.eq_ignore_ascii_case("dvh")
            || unit_name.eq_ignore_ascii_case("dvi")
            || unit_name.eq_ignore_ascii_case("dvb")
            || unit_name.eq_ignore_ascii_case("dvmin")
            || unit_name.eq_ignore_ascii_case("dvmax")
            || unit_name.eq_ignore_ascii_case("cm")
            || unit_name.eq_ignore_ascii_case("mm")
            || unit_name.eq_ignore_ascii_case("Q")
            || unit_name.eq_ignore_ascii_case("in")
            || unit_name.eq_ignore_ascii_case("pc")
            || unit_name.eq_ignore_ascii_case("pt")
        {
            DimensionKind::Length
        } else if unit_name.eq_ignore_ascii_case("deg")
            || unit_name.eq_ignore_ascii_case("grad")
            || unit_name.eq_ignore_ascii_case("rad")
            || unit_name.eq_ignore_ascii_case("turn")
        {
            DimensionKind::Angle
        } else if unit_name.eq_ignore_ascii_case("s") || unit_name.eq_ignore_ascii_case("ms") {
            DimensionKind::Duration
        } else if unit_name.eq_ignore_ascii_case("Hz") || unit_name.eq_ignore_ascii_case("kHz") {
            DimensionKind::Frequency
        } else if unit_name.eq_ignore_ascii_case("dpi")
            || unit_name.eq_ignore_ascii_case("dpcm")
            || unit_name.eq_ignore_ascii_case("dppx")
        {
            DimensionKind::Resolution
        } else if unit_name.eq_ignore_ascii_case("fr") {
            DimensionKind::Flex
        } else {
            DimensionKind::Unknown
        };

        Ok(Dimension {
            value,
            unit,
            kind,
            span,
        })
    }
}

impl<'s> From<(token::Ident<'s>, Span)> for Ident<'s> {
    fn from((token, span): (token::Ident<'s>, Span)) -> Self {
        Ident {
            name: token.name(),
            raw: token.raw,
            span,
        }
    }
}

impl<'s> From<(token::Ident<'s>, Span)> for InterpolableIdentStaticPart<'s> {
    fn from((token, span): (token::Ident<'s>, Span)) -> Self {
        InterpolableIdentStaticPart {
            value: token.name(),
            raw: token.raw,
            span,
        }
    }
}

impl<'s> TryFrom<(token::Number<'s>, Span)> for Number<'s> {
    type Error = Error;

    fn try_from((token, span): (token::Number<'s>, Span)) -> PResult<Self> {
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

impl<'s> From<(token::StrTemplate<'s>, Span)> for InterpolableStrStaticPart<'s> {
    fn from((token, span): (token::StrTemplate<'s>, Span)) -> Self {
        let raw_without_quotes = if token.tail {
            unsafe { token.raw.get_unchecked(0..token.raw.len() - 1) }
        } else if token.head {
            unsafe { token.raw.get_unchecked(1..token.raw.len()) }
        } else {
            token.raw
        };
        let value = if token.escaped {
            util::handle_escape(raw_without_quotes)
        } else {
            Cow::from(raw_without_quotes)
        };
        Self {
            value,
            raw: token.raw,
            span,
        }
    }
}

impl<'s> From<(token::UrlTemplate<'s>, Span)> for InterpolableUrlStaticPart<'s> {
    fn from((token, span): (token::UrlTemplate<'s>, Span)) -> Self {
        let value = if token.escaped {
            util::handle_escape(token.raw)
        } else {
            Cow::from(token.raw)
        };
        Self {
            value,
            raw: token.raw,
            span,
        }
    }
}

impl<'s> From<(token::Str<'s>, Span)> for Str<'s> {
    fn from((str, span): (token::Str<'s>, Span)) -> Self {
        let raw_without_quotes = unsafe { str.raw.get_unchecked(1..str.raw.len() - 1) };
        let value = if str.escaped {
            util::handle_escape(raw_without_quotes)
        } else {
            Cow::from(raw_without_quotes)
        };
        Self {
            value,
            raw: str.raw,
            span,
        }
    }
}
