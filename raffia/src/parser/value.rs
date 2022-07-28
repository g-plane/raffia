use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    Syntax,
};

impl<'a> Parser<'a> {
    pub(super) fn parse_component_value(&mut self) -> PResult<ComponentValue<'a>> {
        match self.tokenizer.peek()? {
            Token::Ident(..) => self
                .parse_interpolable_ident()
                .map(ComponentValue::InterpolableIdent),
            Token::Function(..) => self.parse_function().map(ComponentValue::Function),
            Token::Solidus(..) | Token::Comma(..) | Token::Semicolon(..) => {
                self.parse_delimiter().map(ComponentValue::Delimiter)
            }
            Token::Number(..) => self.parse_number().map(ComponentValue::Number),
            Token::Dimension(..) => self.parse_dimension().map(ComponentValue::Dimension),
            Token::Percentage(..) => self.parse_percentage().map(ComponentValue::Percentage),
            Token::Hash(..) => self.parse_hex_color().map(ComponentValue::HexColor),
            Token::Str(..) => self.parse_str().map(ComponentValue::Str),
            Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss) => {
                self.parse_sass_variable().map(ComponentValue::SassVariable)
            }
            Token::HashLBrace(..) if matches!(self.syntax, Syntax::Scss) => self
                .parse_sass_interpolated_ident()
                .map(ComponentValue::InterpolableIdent),
            token => Err(Error {
                kind: ErrorKind::ExpectComponentValue,
                span: token.span().clone(),
            }),
        }
    }

    fn parse_delimiter(&mut self) -> PResult<Delimiter> {
        use crate::tokenizer::token::*;
        match self.tokenizer.bump()? {
            Token::Solidus(Solidus { span }) => Ok(Delimiter {
                kind: DelimiterKind::Solidus,
                span,
            }),
            Token::Comma(Comma { span }) => Ok(Delimiter {
                kind: DelimiterKind::Comma,
                span,
            }),
            Token::Semicolon(Semicolon { span }) => Ok(Delimiter {
                kind: DelimiterKind::Semicolon,
                span,
            }),
            _ => unreachable!(),
        }
    }

    pub(super) fn parse_dimension(&mut self) -> PResult<Dimension<'a>> {
        let dimension_token = expect!(self, Dimension);
        let unit_name = &dimension_token.unit.name;
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
            Ok(Dimension::Length(Length {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("deg")
            || unit_name.eq_ignore_ascii_case("grad")
            || unit_name.eq_ignore_ascii_case("rad")
            || unit_name.eq_ignore_ascii_case("turn")
        {
            Ok(Dimension::Angle(Angle {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("s") || unit_name.eq_ignore_ascii_case("ms") {
            Ok(Dimension::Duration(Duration {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("Hz") || unit_name.eq_ignore_ascii_case("kHz") {
            Ok(Dimension::Frequency(Frequency {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("dpi")
            || unit_name.eq_ignore_ascii_case("dpcm")
            || unit_name.eq_ignore_ascii_case("dppx")
        {
            Ok(Dimension::Resolution(Resolution {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("fr") {
            Ok(Dimension::Flex(Flex {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else {
            Ok(Dimension::Unknown(UnknownDimension {
                value: dimension_token.value.into(),
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        }
    }

    pub(super) fn parse_function(&mut self) -> PResult<Function<'a>> {
        let func = expect!(self, Function);
        let (args, _) = self.parse_component_values()?;
        let r_paren = expect!(self, RParen);
        Ok(Function {
            name: func.name.into(),
            args,
            span: Span {
                start: func.span.start,
                end: r_paren.span.end,
            },
        })
    }

    pub(super) fn parse_hex_color(&mut self) -> PResult<HexColor<'a>> {
        let token = expect!(self, Hash);
        Ok(HexColor {
            value: token.value,
            raw: token.raw_without_hash,
            span: token.span,
        })
    }

    pub(super) fn parse_ident(&mut self) -> PResult<Ident<'a>> {
        Ok(expect!(self, Ident).into())
    }

    pub(super) fn parse_interpolable_ident(&mut self) -> PResult<InterpolableIdent<'a>> {
        match self.syntax {
            Syntax::Css => self.parse_ident().map(InterpolableIdent::Literal),
            Syntax::Scss => self.parse_sass_interpolated_ident(),
            Syntax::Less => todo!(),
        }
    }

    pub(super) fn parse_number(&mut self) -> PResult<Number<'a>> {
        Ok(expect!(self, Number).into())
    }

    pub(super) fn parse_percentage(&mut self) -> PResult<Percentage<'a>> {
        let token = expect!(self, Percentage);
        Ok(Percentage {
            value: token.value.into(),
            span: token.span,
        })
    }
}
