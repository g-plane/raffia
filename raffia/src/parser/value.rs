use super::{state::QualifiedRuleContext, Parser};
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    util, Syntax,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_component_value(&mut self) -> PResult<ComponentValue<'s>> {
        if matches!(self.syntax, Syntax::Scss | Syntax::Sass) {
            self.parse_sass_bin_expr()
        } else {
            self.parse_component_value_internally()
        }
    }

    pub(super) fn parse_component_value_internally(&mut self) -> PResult<ComponentValue<'s>> {
        match self.tokenizer.peek()? {
            Token::Ident(..) => {
                let ident = self.parse_interpolable_ident()?;
                match self.tokenizer.peek()? {
                    Token::LParen(token) if token.span.start == ident.span().end => {
                        self.parse_function(ident).map(ComponentValue::Function)
                    }
                    _ => Ok(ComponentValue::InterpolableIdent(ident)),
                }
            }
            Token::Solidus(..) | Token::Comma(..) | Token::Semicolon(..) => {
                self.parse_delimiter().map(ComponentValue::Delimiter)
            }
            Token::Number(..) => self.parse_number().map(ComponentValue::Number),
            Token::Dimension(..) => self.parse_dimension().map(ComponentValue::Dimension),
            Token::Percentage(..) => self.parse_percentage().map(ComponentValue::Percentage),
            Token::Hash(..) => self.parse_hex_color().map(ComponentValue::HexColor),
            Token::Str(..) => self
                .parse_str()
                .map(InterpolableStr::Literal)
                .map(ComponentValue::InterpolableStr),
            Token::UrlPrefix(..) => self.parse_url().map(ComponentValue::Url),
            Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.parse_sass_variable().map(ComponentValue::SassVariable)
            }
            Token::LParen(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse_sass_parenthesized_expression()
                .map(ComponentValue::SassParenthesizedExpression),
            Token::HashLBrace(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse_sass_interpolated_ident()
                .map(ComponentValue::InterpolableIdent),
            Token::StrTemplate(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse_sass_interpolated_str()
                .map(InterpolableStr::SassInterpolated)
                .map(ComponentValue::InterpolableStr),
            Token::AtKeyword(..) if self.syntax == Syntax::Less => {
                self.parse_less_variable().map(ComponentValue::LessVariable)
            }
            Token::StrTemplate(..) if self.syntax == Syntax::Less => self
                .parse_less_interpolated_str()
                .map(InterpolableStr::LessInterpolated)
                .map(ComponentValue::InterpolableStr),
            token => Err(Error {
                kind: ErrorKind::ExpectComponentValue,
                span: token.span().clone(),
            }),
        }
    }

    pub(in crate::parser) fn parse_component_values(
        &mut self,
        allow_comma: bool,
    ) -> PResult<ComponentValues<'s>> {
        let first = self.parse_component_value()?;
        let mut span = first.span().clone();

        let mut values = Vec::with_capacity(4);
        values.push(first);
        loop {
            match self.tokenizer.peek()? {
                Token::RBrace(..)
                | Token::RParen(..)
                | Token::Semicolon(..)
                | Token::Dedent(..)
                | Token::Linebreak(..)
                | Token::Eof(..) => break,
                Token::Comma(..) => {
                    if allow_comma {
                        values.push(self.parse_delimiter().map(ComponentValue::Delimiter)?);
                    } else {
                        break;
                    }
                }
                _ => values.push(self.parse_component_value()?),
            }
        }

        if let Some(last) = values.last() {
            span.end = last.span().end;
        }
        Ok(ComponentValues { values, span })
    }

    pub(super) fn parse_dashed_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        match self.parse_interpolable_ident()? {
            // this should be recoverable
            InterpolableIdent::Literal(ident) if util::is_css_wide_keyword(&ident.name) => {
                Err(Error {
                    kind: ErrorKind::CSSWideKeywordDisallowed,
                    span: ident.span,
                })
            }
            ident => Ok(ident),
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

    pub(super) fn parse_dimension(&mut self) -> PResult<Dimension<'s>> {
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

    pub(super) fn parse_function(&mut self, name: InterpolableIdent<'s>) -> PResult<Function<'s>> {
        expect!(self, LParen);
        let values = match self.tokenizer.peek()? {
            Token::RParen(..) => vec![],
            _ => self.parse_component_values(/* allow_comma */ true)?.values,
        };
        let r_paren = expect!(self, RParen);
        let span = Span {
            start: name.span().start,
            end: r_paren.span.end,
        };
        Ok(Function {
            name,
            args: values,
            span,
        })
    }

    pub(super) fn parse_hex_color(&mut self) -> PResult<HexColor<'s>> {
        let token = expect!(self, Hash);
        Ok(HexColor {
            value: token.value,
            raw: token.raw_without_hash,
            span: token.span,
        })
    }

    pub(super) fn parse_ident(&mut self) -> PResult<Ident<'s>> {
        Ok(expect!(self, Ident).into())
    }

    pub(super) fn parse_interpolable_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        match self.syntax {
            Syntax::Css => self.parse_ident().map(InterpolableIdent::Literal),
            Syntax::Scss | Syntax::Sass => self.parse_sass_interpolated_ident(),
            Syntax::Less => {
                // Less variable interpolation is disallowed in declaration value
                if matches!(
                    self.state.qualified_rule_ctx,
                    Some(QualifiedRuleContext::Selector | QualifiedRuleContext::DeclarationName)
                ) {
                    self.parse_less_interpolated_ident()
                } else {
                    self.parse_ident().map(InterpolableIdent::Literal)
                }
            }
        }
    }

    pub(super) fn parse_interpolable_str(&mut self) -> PResult<InterpolableStr<'s>> {
        match self.tokenizer.peek()? {
            Token::Str(..) => self.parse_str().map(InterpolableStr::Literal),
            Token::StrTemplate(token) => match self.syntax {
                Syntax::Scss | Syntax::Sass => self
                    .parse_sass_interpolated_str()
                    .map(InterpolableStr::SassInterpolated),
                Syntax::Less => self
                    .parse_less_interpolated_str()
                    .map(InterpolableStr::LessInterpolated),
                Syntax::Css => Err(Error {
                    kind: ErrorKind::UnexpectedTemplateInCss,
                    span: token.span,
                }),
            },
            token => Err(Error {
                kind: ErrorKind::ExpectString,
                span: token.span().clone(),
            }),
        }
    }

    pub(super) fn parse_number(&mut self) -> PResult<Number<'s>> {
        Ok(expect!(self, Number).into())
    }

    pub(super) fn parse_percentage(&mut self) -> PResult<Percentage<'s>> {
        let token = expect!(self, Percentage);
        Ok(Percentage {
            value: token.value.into(),
            span: token.span,
        })
    }

    pub(super) fn parse_ratio(&mut self, numerator: Number<'s>) -> PResult<Ratio<'s>> {
        expect!(self, Solidus);
        let denominator = self.parse_number()?;
        if denominator.value <= 0.0 {
            // this should be recoverable
            return Err(Error {
                kind: ErrorKind::InvalidRatioDenominator,
                span: denominator.span,
            });
        }

        let span = Span {
            start: numerator.span.start,
            end: denominator.span.end,
        };
        Ok(Ratio {
            numerator,
            denominator,
            span,
        })
    }

    pub(super) fn parse_str(&mut self) -> PResult<Str<'s>> {
        Ok(expect!(self, Str).into())
    }

    pub(super) fn parse_url(&mut self) -> PResult<Url<'s>> {
        let prefix = expect!(self, UrlPrefix);
        match self.tokenizer.peek()? {
            Token::UrlRaw(..) => {
                let value = self.parse_url_raw()?;
                let span = Span {
                    start: prefix.span.start,
                    end: value.span.end + 1, // `)` is consumed, but span excludes it
                };
                Ok(Url {
                    ident: prefix.ident.into(),
                    value: UrlValue::Raw(value),
                    span,
                })
            }
            Token::Str(..) | Token::StrTemplate(..) => {
                let value = self.parse_interpolable_str()?;
                let r_paren = expect!(self, RParen);
                let span = Span {
                    start: prefix.span.start,
                    end: r_paren.span.end,
                };
                Ok(Url {
                    ident: prefix.ident.into(),
                    value: UrlValue::Str(value),
                    span,
                })
            }
            Token::UrlTemplate(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                let value = self.parse_sass_interpolated_url()?;
                let span = Span {
                    start: prefix.span.start,
                    end: value.span.end + 1, // `)` is consumed, but span excludes it
                };
                Ok(Url {
                    ident: prefix.ident.into(),
                    value: UrlValue::SassInterpolated(value),
                    span,
                })
            }
            token => Err(Error {
                kind: ErrorKind::ExpectUrl,
                span: token.span().clone(),
            }),
        }
    }

    fn parse_url_raw(&mut self) -> PResult<UrlRaw<'s>> {
        let url = expect!(self, UrlRaw);
        Ok(UrlRaw {
            value: url.value,
            raw: url.raw,
            span: url.span,
        })
    }
}
