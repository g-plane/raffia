use crate::{
    ast::{
        Dimension, Ident, InterpolableIdentStaticPart, InterpolableStrStaticPart,
        InterpolableUrlStaticPart, Number,
    },
    error::{Error, ErrorKind, PResult},
    tokenizer::token,
    util::{handle_escape, CowStr},
    Span,
};

impl<'s> Dimension<'s> {
    pub(super) fn try_from_token(token: token::Dimension<'s>, span: Span) -> PResult<Self> {
        use crate::ast::{Angle, Duration, Flex, Frequency, Length, Resolution, UnknownDimension};

        let value_span = Span {
            start: span.start,
            end: span.start + token.value.raw.len(),
        };
        let unit_span = Span {
            start: span.start + token.value.raw.len(),
            end: span.end,
        };
        let value = Number::try_from_token(token.value, value_span)?;
        let unit = Ident::from_token(token.unit, unit_span);
        let unit_name = &unit.name;
        if unit_name.eq_ignore_ascii_case("px")
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
            Ok(Dimension::Length(Length { value, unit, span }))
        } else if unit_name.eq_ignore_ascii_case("deg")
            || unit_name.eq_ignore_ascii_case("grad")
            || unit_name.eq_ignore_ascii_case("rad")
            || unit_name.eq_ignore_ascii_case("turn")
        {
            Ok(Dimension::Angle(Angle { value, unit, span }))
        } else if unit_name.eq_ignore_ascii_case("s") || unit_name.eq_ignore_ascii_case("ms") {
            Ok(Dimension::Duration(Duration { value, unit, span }))
        } else if unit_name.eq_ignore_ascii_case("Hz") || unit_name.eq_ignore_ascii_case("kHz") {
            Ok(Dimension::Frequency(Frequency { value, unit, span }))
        } else if unit_name.eq_ignore_ascii_case("dpi")
            || unit_name.eq_ignore_ascii_case("dpcm")
            || unit_name.eq_ignore_ascii_case("dppx")
        {
            Ok(Dimension::Resolution(Resolution { value, unit, span }))
        } else if unit_name.eq_ignore_ascii_case("fr") {
            Ok(Dimension::Flex(Flex { value, unit, span }))
        } else {
            Ok(Dimension::Unknown(UnknownDimension { value, unit, span }))
        }
    }
}

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
