use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_less_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let first = match self.tokenizer.peek()? {
            Token::Ident(..) => {
                let ident = expect!(self, Ident);
                match self.tokenizer.peek()? {
                    Token::AtLBraceVar(token) if ident.span.end == token.span.start => {
                        LessInterpolatedIdentElement::Static(ident.into())
                    }
                    _ => return Ok(InterpolableIdent::Literal(ident.into())),
                }
            }
            Token::AtLBraceVar(..) => self
                .parse_less_variable_interpolation()
                .map(LessInterpolatedIdentElement::Variable)?,
            _ => unreachable!(),
        };
        let mut span = first.span().clone();

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match self.tokenizer.peek()? {
                Token::Ident(token) if span.end == token.span.start => {
                    let ident = expect!(self, Ident);
                    span.end = ident.span.end;
                    elements.push(LessInterpolatedIdentElement::Static(ident.into()));
                }
                Token::AtLBraceVar(token) if span.end == token.span.start => {
                    let variable = self.parse_less_variable_interpolation()?;
                    span.end = variable.span.end;
                    elements.push(LessInterpolatedIdentElement::Variable(variable));
                }
                _ => break,
            }
        }

        Ok(InterpolableIdent::LessInterpolated(LessInterpolatedIdent {
            elements,
            span,
        }))
    }

    pub(super) fn parse_less_interpolated_str(&mut self) -> PResult<LessInterpolatedStr<'s>> {
        let first = expect!(self, StrTemplate);
        let mut span = first.span.clone();
        let mut elements = vec![LessInterpolatedStrElement::Static(
            InterpolableStrStaticPart {
                value: first.value,
                raw: first.raw,
                span: first.span,
            },
        )];

        loop {
            match self.tokenizer.bump()? {
                Token::StrTemplate(token) => {
                    let end = token.span.end;
                    elements.push(LessInterpolatedStrElement::Static(
                        InterpolableStrStaticPart {
                            value: token.value,
                            raw: token.raw,
                            span: token.span,
                        },
                    ));
                    if token.tail {
                        span.end = end;
                        break;
                    }
                }
                // '@' is consumed, so '{' left only
                Token::LBrace(l_brace) => {
                    let name = self.parse_ident()?;
                    self.assert_no_ws_or_comment(&l_brace.span, &name.span)?;

                    let r_brace = expect!(self, RBrace);
                    elements.push(LessInterpolatedStrElement::Variable(
                        LessVariableInterpolation {
                            name,
                            span: Span {
                                start: l_brace.span.start - 1,
                                end: r_brace.span.end,
                            },
                        },
                    ));
                }
                token => {
                    return Err(Error {
                        kind: ErrorKind::Unexpected("StrTemplate or LBrace"),
                        span: token.span().clone(),
                    })
                }
            }
        }

        Ok(LessInterpolatedStr { elements, span })
    }

    pub(super) fn parse_less_property_merge(&mut self) -> PResult<Option<LessPropertyMerge>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        match self.tokenizer.peek()? {
            Token::Plus(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(LessPropertyMerge {
                    kind: LessPropertyMergeKind::Comma,
                    span: token.span,
                }))
            }
            Token::PlusUnderscore(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(LessPropertyMerge {
                    kind: LessPropertyMergeKind::Space,
                    span: token.span,
                }))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn parse_less_variable(&mut self) -> PResult<LessVariable<'s>> {
        let at_keyword = expect!(self, AtKeyword);
        Ok(LessVariable {
            name: at_keyword.ident.into(),
            span: at_keyword.span,
        })
    }

    pub(super) fn parse_less_variable_declaration(
        &mut self,
    ) -> PResult<LessVariableDeclaration<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let name = self.parse_less_variable()?;
        expect!(self, Colon);
        let value = self.parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(LessVariableDeclaration { name, value, span })
    }

    fn parse_less_variable_interpolation(&mut self) -> PResult<LessVariableInterpolation<'s>> {
        let at_lbrace_var = expect!(self, AtLBraceVar);
        Ok(LessVariableInterpolation {
            name: at_lbrace_var.ident.into(),
            span: at_lbrace_var.span,
        })
    }
}
