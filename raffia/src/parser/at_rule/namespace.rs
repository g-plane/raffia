use super::Parser;
use crate::{ast::*, error::PResult, tokenizer::Token, Spanned};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_namespace_prelude(&mut self) -> PResult<NamespacePrelude<'s>> {
        let prefix = match self.tokenizer.peek()? {
            Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                self.parse_interpolable_ident().map(Some)?
            }
            _ => None,
        };
        let uri = match self.tokenizer.peek()? {
            Token::UrlPrefix(..) => self.parse_url().map(NamespacePreludeUri::Url)?,
            _ => self
                .parse_interpolable_str()
                .map(NamespacePreludeUri::Str)?,
        };

        let mut span = uri.span().clone();
        if let Some(prefix) = &prefix {
            span.start = prefix.span().start;
        }
        Ok(NamespacePrelude { prefix, uri, span })
    }
}
