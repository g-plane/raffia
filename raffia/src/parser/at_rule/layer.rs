use super::Parser;
use crate::{
    ast::*,
    error::{Error, PResult},
    pos::{Span, Spanned},
    tokenizer::Token,
    util,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_layer_name(&mut self) -> PResult<LayerName<'s>> {
        let first = self.parse_interpolable_ident()?;
        let start = first.span().start;
        let mut end = first.span().end;

        let mut idents = vec![first];
        while let Token::Dot(dot) = self.tokenizer.peek()? {
            if dot.span.start == end {
                self.tokenizer.bump()?;
                let ident = self.parse_interpolable_ident()?;
                self.assert_no_ws_or_comment(&dot.span, ident.span())?;
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
