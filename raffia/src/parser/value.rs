use super::{state::QualifiedRuleContext, Parser};
use crate::{
    ast::*,
    bump, eat,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::{handle_escape, Token},
    util::LastOfNonEmpty,
    Parse, Syntax,
};
use std::borrow::Cow;

const PRECEDENCE_MULTIPLY: u8 = 2;
const PRECEDENCE_PLUS: u8 = 1;

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(in crate::parser) fn parse_calc_expr(&mut self) -> PResult<ComponentValue<'s>> {
        self.parse_calc_expr_recursively(0)
    }

    fn parse_calc_expr_recursively(&mut self, precedence: u8) -> PResult<ComponentValue<'s>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            if eat!(self, LParen).is_some() {
                let expr = self.parse_calc_expr()?;
                expect!(self, RParen);
                expr
            } else {
                self.parse_component_value_atom()?
            }
        } else {
            self.parse_calc_expr_recursively(precedence + 1)?
        };

        loop {
            let operator = match peek!(self) {
                Token::Asterisk(..) if precedence == PRECEDENCE_MULTIPLY => {
                    let token = expect!(self, Asterisk);
                    CalcOperator {
                        kind: CalcOperatorKind::Multiply,
                        span: token.span,
                    }
                }
                Token::Solidus(..) if precedence == PRECEDENCE_MULTIPLY => {
                    let token = expect!(self, Solidus);
                    CalcOperator {
                        kind: CalcOperatorKind::Division,
                        span: token.span,
                    }
                }
                Token::Plus(..) if precedence == PRECEDENCE_PLUS => {
                    let token = expect!(self, Plus);
                    CalcOperator {
                        kind: CalcOperatorKind::Plus,
                        span: token.span,
                    }
                }
                Token::Minus(..) if precedence == PRECEDENCE_PLUS => {
                    let token = expect!(self, Minus);
                    CalcOperator {
                        kind: CalcOperatorKind::Minus,
                        span: token.span,
                    }
                }
                _ => break,
            };

            let right = self.parse_calc_expr_recursively(precedence + 1)?;
            let span = Span {
                start: left.span().start,
                end: right.span().end,
            };
            left = ComponentValue::Calc(Calc {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    pub(super) fn parse_component_value_atom(&mut self) -> PResult<ComponentValue<'s>> {
        match peek!(self) {
            Token::Ident(..) => {
                let ident = self.parse::<InterpolableIdent>()?;
                let ident_end = ident.span().end;
                match peek!(self) {
                    Token::LParen(token) if token.span.start == ident_end => match ident {
                        InterpolableIdent::Literal(ident)
                            if ident.name.eq_ignore_ascii_case("src") =>
                        {
                            self.parse_src_url(ident).map(ComponentValue::Url)
                        }
                        ident => self.parse_function(ident).map(ComponentValue::Function),
                    },
                    Token::Dot(token) if token.span.start == ident_end => {
                        if let InterpolableIdent::Literal(namespace) = ident {
                            self.parse_sass_namespaced_expression(namespace)
                                .map(ComponentValue::SassNamespacedExpression)
                        } else {
                            Ok(ComponentValue::InterpolableIdent(ident))
                        }
                    }
                    _ => Ok(ComponentValue::InterpolableIdent(ident)),
                }
            }
            Token::Solidus(..) | Token::Comma(..) => self.parse().map(ComponentValue::Delimiter),
            Token::Number(..) => self.parse().map(ComponentValue::Number),
            Token::Dimension(..) => self.parse().map(ComponentValue::Dimension),
            Token::Percentage(..) => self.parse().map(ComponentValue::Percentage),
            Token::Hash(..) => self.parse().map(ComponentValue::HexColor),
            Token::Str(..) => self
                .parse()
                .map(InterpolableStr::Literal)
                .map(ComponentValue::InterpolableStr),
            Token::UrlPrefix(..) => self.parse().map(ComponentValue::Url),
            Token::LBracket(..) => self.parse().map(ComponentValue::BracketBlock),
            Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.parse().map(ComponentValue::SassVariable)
            }
            Token::LParen(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse()
                .map(ComponentValue::SassParenthesizedExpression),
            Token::HashLBrace(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse_sass_interpolated_ident()
                .map(ComponentValue::InterpolableIdent),
            Token::StrTemplate(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse()
                .map(InterpolableStr::SassInterpolated)
                .map(ComponentValue::InterpolableStr),
            Token::AtKeyword(..) if self.syntax == Syntax::Less => {
                self.parse().map(ComponentValue::LessVariable)
            }
            Token::StrTemplate(..) if self.syntax == Syntax::Less => self
                .parse()
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
        allow_semicolon: bool,
    ) -> PResult<ComponentValues<'s>> {
        let first = self.parse::<ComponentValue>()?;
        let mut span = first.span().clone();

        let mut values = Vec::with_capacity(4);
        values.push(first);
        loop {
            match peek!(self) {
                Token::RBrace(..)
                | Token::RParen(..)
                | Token::Dedent(..)
                | Token::Linebreak(..)
                | Token::Exclamation(..)
                | Token::Eof(..) => break,
                Token::Semicolon(..) => {
                    if allow_semicolon {
                        values.push(self.parse().map(ComponentValue::Delimiter)?);
                    } else {
                        break;
                    }
                }
                Token::Comma(..) => {
                    if allow_comma {
                        values.push(self.parse().map(ComponentValue::Delimiter)?);
                    } else {
                        break;
                    }
                }
                _ => values.push(self.parse()?),
            }
        }

        span.end = values.last_of_non_empty().span().end;
        Ok(ComponentValues { values, span })
    }

    pub(super) fn parse_dashed_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        let ident = self.parse()?;
        match &ident {
            InterpolableIdent::Literal(ident) if !ident.name.starts_with("--") => {
                self.recoverable_errors.push(Error {
                    kind: ErrorKind::ExpectDashedIdent,
                    span: ident.span.clone(),
                });
            }
            _ => {}
        }
        Ok(ident)
    }

    pub(super) fn parse_function(&mut self, name: InterpolableIdent<'s>) -> PResult<Function<'s>> {
        expect!(self, LParen);
        let values = match peek!(self) {
            Token::RParen(..) => vec![],
            _ => match &name {
                InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("calc") => {
                    vec![self.parse_calc_expr()?]
                }
                _ => {
                    self.parse_component_values(
                        /* allow_comma */ true, /* allow_semicolon */ true,
                    )?
                    .values
                }
            },
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

    pub(super) fn parse_ratio(&mut self, numerator: Number<'s>) -> PResult<Ratio<'s>> {
        expect!(self, Solidus);
        let denominator = self.parse::<Number>()?;
        if denominator.value <= 0.0 {
            self.recoverable_errors.push(Error {
                kind: ErrorKind::InvalidRatioDenominator,
                span: denominator.span.clone(),
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

    fn parse_src_url(&mut self, name: Ident<'s>) -> PResult<Url<'s>> {
        // caller of `parse_src_url` should make sure there're no whitespaces before paren
        expect!(self, LParen);
        let value = match peek!(self) {
            Token::Str(..) | Token::StrTemplate(..) => {
                Some(UrlValue::Str(self.parse::<InterpolableStr>()?))
            }
            _ => None,
        };
        let modifiers = match peek!(self) {
            Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                let mut modifiers = Vec::with_capacity(1);
                loop {
                    modifiers.push(self.parse()?);
                    if let Token::RParen(..) = peek!(self) {
                        break;
                    }
                }
                modifiers
            }
            _ => vec![],
        };
        let r_paren = expect!(self, RParen);
        let span = Span {
            start: name.span.start,
            end: r_paren.span.end,
        };
        Ok(Url {
            name,
            value,
            modifiers,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for BracketBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let l_bracket = expect!(input, LBracket);
        let mut value = Vec::with_capacity(3);
        loop {
            match peek!(input) {
                Token::RBracket(..) => break,
                _ => value.push(input.parse()?),
            }
        }
        let r_bracket = expect!(input, RBracket);
        Ok(BracketBlock {
            value,
            span: Span {
                start: l_bracket.span.start,
                end: r_bracket.span.end,
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComponentValue<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if matches!(input.syntax, Syntax::Scss | Syntax::Sass) {
            input.parse_sass_bin_expr()
        } else {
            input.parse_component_value_atom()
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComponentValues<'s> {
    /// This is for public-use only. For internal code of Raffia, **DO NOT** use.
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        input.parse_component_values(/* allow_comma */ true, /* allow_semicolon */ true)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Delimiter {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        use crate::tokenizer::token::*;
        match bump!(input) {
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
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Dimension<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let dimension_token = expect!(input, Dimension);
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
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("deg")
            || unit_name.eq_ignore_ascii_case("grad")
            || unit_name.eq_ignore_ascii_case("rad")
            || unit_name.eq_ignore_ascii_case("turn")
        {
            Ok(Dimension::Angle(Angle {
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("s") || unit_name.eq_ignore_ascii_case("ms") {
            Ok(Dimension::Duration(Duration {
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("Hz") || unit_name.eq_ignore_ascii_case("kHz") {
            Ok(Dimension::Frequency(Frequency {
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("dpi")
            || unit_name.eq_ignore_ascii_case("dpcm")
            || unit_name.eq_ignore_ascii_case("dppx")
        {
            Ok(Dimension::Resolution(Resolution {
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else if unit_name.eq_ignore_ascii_case("fr") {
            Ok(Dimension::Flex(Flex {
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        } else {
            Ok(Dimension::Unknown(UnknownDimension {
                value: dimension_token.value.try_into()?,
                unit: dimension_token.unit.into(),
                span: dimension_token.span,
            }))
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for HexColor<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token = expect!(input, Hash);
        let raw = token.raw_without_hash;
        let value = if token.escaped {
            handle_escape(raw).map_err(|kind| Error {
                kind,
                span: token.span.clone(),
            })?
        } else {
            Cow::from(raw)
        };
        Ok(HexColor {
            value,
            raw,
            span: token.span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Ident<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        Ok(expect!(input, Ident).into())
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for InterpolableIdent<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.syntax {
            Syntax::Css => input.parse().map(InterpolableIdent::Literal),
            Syntax::Scss | Syntax::Sass => input.parse_sass_interpolated_ident(),
            Syntax::Less => {
                // Less variable interpolation is disallowed in declaration value
                if matches!(
                    input.state.qualified_rule_ctx,
                    Some(QualifiedRuleContext::Selector | QualifiedRuleContext::DeclarationName)
                ) {
                    input.parse_less_interpolated_ident()
                } else {
                    input.parse().map(InterpolableIdent::Literal)
                }
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for InterpolableStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match peek!(input) {
            Token::Str(..) => input.parse().map(InterpolableStr::Literal),
            Token::StrTemplate(token) => match input.syntax {
                Syntax::Scss | Syntax::Sass => input.parse().map(InterpolableStr::SassInterpolated),
                Syntax::Less => input.parse().map(InterpolableStr::LessInterpolated),
                Syntax::Css => Err(Error {
                    kind: ErrorKind::UnexpectedTemplateInCss,
                    span: token.span.clone(),
                }),
            },
            token => Err(Error {
                kind: ErrorKind::ExpectString,
                span: token.span().clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Number<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        expect!(input, Number).try_into()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Percentage<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token = expect!(input, Percentage);
        Ok(Percentage {
            value: token.value.try_into()?,
            span: token.span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Str<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        expect!(input, Str).try_into()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Url<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let prefix = expect!(input, UrlPrefix);
        if prefix.is_raw {
            if let Ok(value) = input.try_parse(|parser| parser.parse::<UrlRaw>()) {
                let span = Span {
                    start: prefix.span.start,
                    end: value.span.end + 1, // `)` is consumed, but span excludes it
                };
                Ok(Url {
                    name: prefix.ident.into(),
                    value: Some(UrlValue::Raw(value)),
                    modifiers: vec![],
                    span,
                })
            } else if matches!(input.syntax, Syntax::Scss | Syntax::Sass) {
                let value = input.parse::<SassInterpolatedUrl>()?;
                let span = Span {
                    start: prefix.span.start,
                    end: value.span.end + 1, // `)` is consumed, but span excludes it
                };
                Ok(Url {
                    name: prefix.ident.into(),
                    value: Some(UrlValue::SassInterpolated(value)),
                    modifiers: vec![],
                    span,
                })
            } else {
                Err(Error {
                    kind: ErrorKind::ExpectUrl,
                    span: bump!(input).span().clone(),
                })
            }
        } else {
            let value = input.parse()?;
            let modifiers = match peek!(input) {
                Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                    let mut modifiers = Vec::with_capacity(1);
                    loop {
                        modifiers.push(input.parse()?);
                        if let Token::RParen(..) = peek!(input) {
                            break;
                        }
                    }
                    modifiers
                }
                _ => vec![],
            };
            let r_paren = expect!(input, RParen);
            let span = Span {
                start: prefix.span.start,
                end: r_paren.span.end,
            };
            Ok(Url {
                name: prefix.ident.into(),
                value: Some(UrlValue::Str(value)),
                modifiers,
                span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for UrlModifier<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse::<InterpolableIdent>()?;
        match peek!(input) {
            Token::LParen(l_paren) if ident.span().end == l_paren.span.start => {
                input.parse_function(ident).map(UrlModifier::Function)
            }
            _ => Ok(UrlModifier::Ident(ident)),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for UrlRaw<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.scan_url_raw_or_template()? {
            Token::UrlRaw(url) => {
                let value = if url.escaped {
                    handle_escape(url.raw).map_err(|kind| Error {
                        kind,
                        span: url.span.clone(),
                    })?
                } else {
                    Cow::from(url.raw)
                };
                Ok(UrlRaw {
                    value,
                    raw: url.raw,
                    span: url.span,
                })
            }
            token => Err(Error {
                kind: ErrorKind::Unexpected("<url>", token.symbol()),
                span: token.span().clone(),
            }),
        }
    }
}
