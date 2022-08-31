use super::Parser;
use crate::{ast::*, error::PResult, peek, tokenizer::Token, Parse, Spanned};

// https://drafts.csswg.org/css-fonts/Overview.bs
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for FontFamilyName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match peek!(input) {
            Token::Str(..) | Token::StrTemplate(..) => input.parse().map(FontFamilyName::Str),
            _ => {
                let first = input.parse::<InterpolableIdent>()?;
                let mut span = first.span().clone();

                let mut idents = vec![first];
                while let Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) =
                    peek!(input)
                {
                    idents.push(input.parse()?);
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
