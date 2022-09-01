use super::Parser;
use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::Token,
    Parse,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_less_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let first = match peek!(self) {
            Token::Ident(..) => {
                let ident = expect!(self, Ident);
                match peek!(self) {
                    Token::AtLBraceVar(token) if ident.span.end == token.span.start => {
                        LessInterpolatedIdentElement::Static(ident.into())
                    }
                    _ => return Ok(InterpolableIdent::Literal(ident.into())),
                }
            }
            Token::AtLBraceVar(..) => self.parse().map(LessInterpolatedIdentElement::Variable)?,
            _ => unreachable!(),
        };
        let mut span = first.span().clone();

        let mut elements = Vec::with_capacity(4);
        elements.push(first);
        loop {
            match peek!(self) {
                Token::Ident(token) if span.end == token.span.start => {
                    let ident = expect!(self, Ident);
                    span.end = ident.span.end;
                    elements.push(LessInterpolatedIdentElement::Static(ident.into()));
                }
                Token::AtLBraceVar(token) if span.end == token.span.start => {
                    let variable = self.parse::<LessVariableInterpolation>()?;
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
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessInterpolatedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = expect!(input, StrTemplate);
        let quote = first.raw.chars().next().unwrap();
        debug_assert!(quote == '\'' || quote == '"');
        let mut span = first.span.clone();
        let mut elements = vec![LessInterpolatedStrElement::Static(first.try_into()?)];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let token = input.tokenizer.scan_string_template(quote)?;
                let tail = token.tail;
                let end = token.span.end;
                elements.push(LessInterpolatedStrElement::Static(token.try_into()?));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '@' is consumed, so '{' left only
                let l_brace = expect!(input, LBrace);
                let name = expect_without_ws_or_comments!(input, Ident).into();

                let r_brace = expect!(input, RBrace);
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
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(LessInterpolatedStr { elements, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Option<LessPropertyMerge> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        match peek!(input) {
            Token::Plus(..) => {
                let token = expect!(input, Plus);
                Ok(Some(LessPropertyMerge {
                    kind: LessPropertyMergeKind::Comma,
                    span: token.span,
                }))
            }
            Token::PlusUnderscore(..) => {
                let token = expect!(input, PlusUnderscore);
                Ok(Some(LessPropertyMerge {
                    kind: LessPropertyMergeKind::Space,
                    span: token.span,
                }))
            }
            _ => Ok(None),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let at_keyword = expect!(input, AtKeyword);
        Ok(LessVariable {
            name: at_keyword.ident.into(),
            span: at_keyword.span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let name = input.parse::<LessVariable>()?;
        expect!(input, Colon);
        let value = input.parse_component_values(
            /* allow_comma */ true, /* allow_semicolon */ false,
        )?;

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(LessVariableDeclaration { name, value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableInterpolation<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let at_lbrace_var = expect!(input, AtLBraceVar);
        Ok(LessVariableInterpolation {
            name: at_lbrace_var.ident.into(),
            span: at_lbrace_var.span,
        })
    }
}
