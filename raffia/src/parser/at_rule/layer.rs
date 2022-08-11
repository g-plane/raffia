use super::Parser;
use crate::{
    ast::*,
    error::PResult,
    pos::{Span, Spanned},
    tokenizer::Token,
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

        let span = Span { start, end };
        Ok(LayerName { idents, span })
    }
}
