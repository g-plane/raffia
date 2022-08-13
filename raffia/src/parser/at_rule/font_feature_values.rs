use super::Parser;
use crate::{ast::*, error::PResult, tokenizer::Token, Parse, Spanned};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for FontFamilyName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Str(..) | Token::StrTemplate(..) => input.parse().map(FontFamilyName::Str),
            _ => {
                let first = input.parse::<InterpolableIdent>()?;
                let mut span = first.span().clone();

                let mut idents = vec![first];
                loop {
                    match input.tokenizer.peek()? {
                        Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                            idents.push(input.parse()?);
                        }
                        _ => break,
                    }
                }
                if let Some(last) = idents.last() {
                    span.end = last.span().end;
                }
                Ok(FontFamilyName::Unquoted(UnquotedFontFamilyName {
                    idents,
                    span,
                }))
            }
        }
    }
}
