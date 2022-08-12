use super::Parser;
use crate::{ast::*, eat, error::PResult, tokenizer::Token, Spanned};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_document_prelude(&mut self) -> PResult<DocumentPrelude<'s>> {
        let first = self.parse_document_prelude_matcher()?;
        let mut span = first.span().clone();

        let mut matchers = vec![first];
        while eat!(self, Comma).is_some() {
            matchers.push(self.parse_document_prelude_matcher()?);
        }
        if let Some(last) = matchers.last() {
            span.end = last.span().end;
        }
        Ok(DocumentPrelude { matchers, span })
    }

    fn parse_document_prelude_matcher(&mut self) -> PResult<DocumentPreludeMatcher<'s>> {
        match self.tokenizer.peek()? {
            Token::UrlPrefix(..) => self.parse_url().map(DocumentPreludeMatcher::Url),
            _ => {
                let name = self.parse_interpolable_ident()?;
                let next_token = self.tokenizer.peek()?;
                self.assert_no_ws_or_comment(name.span(), next_token.span())?;
                self.parse_function(name)
                    .map(DocumentPreludeMatcher::Function)
            }
        }
    }
}
