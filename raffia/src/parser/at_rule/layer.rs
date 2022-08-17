use super::Parser;
use crate::{
    ast::*,
    error::{Error, PResult},
    pos::{Span, Spanned},
    tokenizer::Token,
    util, Parse,
};

// https://drafts.csswg.org/css-cascade-5/#layer-names
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LayerName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<InterpolableIdent>()?;
        let start = first.span().start;
        let mut end = first.span().end;

        let mut idents = vec![first];
        while let Token::Dot(dot) = input.tokenizer.peek()? {
            if dot.span.start == end {
                input.tokenizer.bump()?;
                let ident = input.parse::<InterpolableIdent>()?;
                input.assert_no_ws_or_comment(&dot.span, ident.span())?;
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
            // this should be recoverable
            return Err(Error {
                kind: crate::error::ErrorKind::CSSWideKeywordDisallowed,
                span: invalid_ident.span().clone(),
            });
        }

        let span = Span { start, end };
        Ok(LayerName { idents, span })
    }
}
