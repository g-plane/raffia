use super::Parser;
use crate::{
    ast::*,
    bump,
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util, Parse,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_less_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let first = match peek!(self) {
            TokenWithSpan {
                token: Token::Ident(..),
                ..
            } => {
                let (ident, ident_span) = expect!(self, Ident);
                match peek!(self) {
                    TokenWithSpan {
                        token: Token::AtLBraceVar(..),
                        span,
                    } if ident_span.end == span.start => {
                        LessInterpolatedIdentElement::Static((ident, ident_span).into())
                    }
                    _ => return Ok(InterpolableIdent::Literal((ident, ident_span).into())),
                }
            }
            TokenWithSpan {
                token: Token::AtLBraceVar(..),
                ..
            } => self.parse().map(LessInterpolatedIdentElement::Variable)?,
            TokenWithSpan { token, span } => {
                use crate::{
                    token::{AtLBraceVar, Ident},
                    tokenizer::TokenSymbol,
                };
                return Err(Error {
                    kind: ErrorKind::ExpectOneOf(
                        vec![Ident::symbol(), AtLBraceVar::symbol()],
                        token.symbol(),
                    ),
                    span: span.clone(),
                });
            }
        };
        let mut span = first.span().clone();

        let mut elements = self.parse_less_interpolated_ident_rest(&mut span.end)?;
        elements.insert(0, first);

        Ok(InterpolableIdent::LessInterpolated(LessInterpolatedIdent {
            elements,
            span,
        }))
    }

    pub(super) fn parse_less_interpolated_ident_rest(
        &mut self,
        end: &mut usize,
    ) -> PResult<Vec<LessInterpolatedIdentElement<'s>>> {
        let mut elements = vec![];
        loop {
            match peek!(self) {
                TokenWithSpan {
                    token: Token::Ident(..),
                    span: ident_span,
                } if *end == ident_span.start => {
                    let (ident, ident_span) = expect!(self, Ident);
                    *end = ident_span.end;
                    elements.push(LessInterpolatedIdentElement::Static(
                        (ident, ident_span).into(),
                    ));
                }
                TokenWithSpan {
                    token: Token::AtLBraceVar(..),
                    span: at_brace_var_span,
                } if *end == at_brace_var_span.start => {
                    let variable = self.parse::<LessVariableInterpolation>()?;
                    *end = variable.span.end;
                    elements.push(LessInterpolatedIdentElement::Variable(variable));
                }
                _ => return Ok(elements),
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessEscapedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, Tilde);
        let str = input.parse::<Str>()?;
        let span = Span {
            start,
            end: str.span.end,
        };
        Ok(LessEscapedStr { str, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessInterpolatedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (first, first_span) = expect!(input, StrTemplate);
        let quote = first.raw.chars().next().unwrap();
        debug_assert!(quote == '\'' || quote == '"');
        let mut span = first_span.clone();
        let mut elements = vec![LessInterpolatedStrElement::Static(
            (first, first_span).into(),
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let (token, str_tpl_span) = input.tokenizer.scan_string_template(quote)?;
                let tail = token.tail;
                let end = str_tpl_span.end;
                elements.push(LessInterpolatedStrElement::Static(
                    (token, str_tpl_span).into(),
                ));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '@' is consumed, so '{' left only
                let start = expect!(input, LBrace).1.start - 1;
                let (name, name_span) = expect_without_ws_or_comments!(input, Ident);

                let end = expect!(input, RBrace).1.end;
                elements.push(LessInterpolatedStrElement::Variable(
                    LessVariableInterpolation {
                        name: (name, name_span).into(),
                        span: Span { start, end },
                    },
                ));
            }
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(LessInterpolatedStr { elements, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Option<LessPropertyMerge> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        match &peek!(input).token {
            Token::Plus(..) => Ok(Some(LessPropertyMerge {
                kind: LessPropertyMergeKind::Comma,
                span: bump!(input).span,
            })),
            Token::PlusUnderscore(..) => Ok(Some(LessPropertyMerge {
                kind: LessPropertyMergeKind::Space,
                span: bump!(input).span,
            })),
            _ => Ok(None),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessPropertyVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (dollar_var, span) = expect!(input, DollarVar);
        Ok(LessPropertyVariable {
            name: Ident::from((
                dollar_var.ident,
                Span {
                    start: span.start + 1,
                    end: span.end,
                },
            )),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (at_keyword, span) = expect!(input, AtKeyword);
        Ok(LessVariable {
            name: (
                at_keyword.ident,
                Span {
                    start: span.start + 1,
                    end: span.end,
                },
            )
                .into(),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let name = input.parse::<LessVariable>()?;
        expect!(input, Colon);
        let value = input.parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(LessVariableDeclaration { name, value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableInterpolation<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (at_lbrace_var, span) = expect!(input, AtLBraceVar);
        Ok(LessVariableInterpolation {
            name: (
                at_lbrace_var.ident,
                Span {
                    start: span.start + 2,
                    end: span.end - 1,
                },
            )
                .into(),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, at_span) = expect!(input, At);
        let variable = input.parse::<LessVariable>()?;
        util::assert_no_ws_or_comment(&at_span, &variable.span)?;

        let span = Span {
            start: at_span.start,
            end: variable.span.end,
        };
        Ok(LessVariableVariable { variable, span })
    }
}
