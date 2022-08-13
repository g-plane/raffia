use super::Parser;
use crate::{ast::*, error::PResult, tokenizer::Token, Parse, Spanned};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for NamespacePrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let prefix = match input.tokenizer.peek()? {
            Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                input.parse::<InterpolableIdent>().map(Some)?
            }
            _ => None,
        };
        let uri = match input.tokenizer.peek()? {
            Token::UrlPrefix(..) => input.parse().map(NamespacePreludeUri::Url)?,
            _ => input.parse().map(NamespacePreludeUri::Str)?,
        };

        let mut span = uri.span().clone();
        if let Some(prefix) = &prefix {
            span.start = prefix.span().start;
        }
        Ok(NamespacePrelude { prefix, uri, span })
    }
}
