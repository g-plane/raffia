use super::Parser;
use crate::{ast::*, error::PResult, tokenizer::Token, Spanned};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_font_feature_values_prelude(&mut self) -> PResult<FontFamilyName<'s>> {
        match self.tokenizer.peek()? {
            Token::Str(..) | Token::StrTemplate(..) => {
                self.parse_interpolable_str().map(FontFamilyName::Str)
            }
            _ => {
                let first = self.parse_interpolable_ident()?;
                let mut span = first.span().clone();

                let mut idents = vec![first];
                loop {
                    match self.tokenizer.peek()? {
                        Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                            idents.push(self.parse_interpolable_ident()?);
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
