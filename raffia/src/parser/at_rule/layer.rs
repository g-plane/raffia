use super::Parser;
use crate::{
    Parse,
    ast::*,
    bump, eat,
    error::{Error, PResult},
    peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util,
};

// https://drafts.csswg.org/css-cascade-5/#layering
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LayerNames<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<LayerName>()?;
        let mut span = first.span.clone();

        let mut names = vec![first];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            names.push(input.parse()?);
        }

        if let Some(last) = names.last() {
            span.end = last.span.end;
        }
        Ok(LayerNames {
            names,
            comma_spans,
            span,
        })
    }
}

// https://drafts.csswg.org/css-cascade-5/#layer-names
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LayerName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<InterpolableIdent>()?;
        let start = first.span().start;
        let mut end = first.span().end;

        let mut idents = vec![first];
        while let TokenWithSpan {
            token: Token::Dot(..),
            span,
        } = peek!(input)
        {
            if span.start == end {
                let span = bump!(input).span;
                let ident = input.parse::<InterpolableIdent>()?;
                util::assert_no_ws_or_comment(&span, ident.span())?;
                end = ident.span().end;
                idents.push(ident);
            } else {
                break;
            }
        }

        let invalid_ident = idents.iter().find(|ident| match &ident {
            InterpolableIdent::Literal(ident) => util::is_css_wide_keyword(&ident.name),
            _ => false,
        });
        if let Some(invalid_ident) = invalid_ident {
            input.recoverable_errors.push(Error {
                kind: crate::error::ErrorKind::CSSWideKeywordDisallowed,
                span: invalid_ident.span().clone(),
            });
        }

        let span = Span { start, end };
        Ok(LayerName { idents, span })
    }
}
