use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    pos::{Span, Spanned},
    tokenizer::Token,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_custom_media(&mut self) -> PResult<CustomMedia<'s>> {
        let name = self.parse_interpolable_ident()?;
        match &name {
            InterpolableIdent::Literal(ident) if !ident.name.starts_with("--") => {
                // this should be recoverable
                return Err(Error {
                    kind: ErrorKind::InvalidExtensionName,
                    span: ident.span.clone(),
                });
            }
            _ => {}
        }

        let value = self.parse_custom_media_value()?;
        let span = Span {
            start: name.span().start,
            end: value.span().end,
        };
        Ok(CustomMedia { name, value, span })
    }

    fn parse_custom_media_value(&mut self) -> PResult<CustomMediaValue<'s>> {
        match self.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("true") => {
                self.parse_ident().map(CustomMediaValue::True)
            }
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("false") => {
                self.parse_ident().map(CustomMediaValue::False)
            }
            _ => self
                .parse_media_query_list()
                .map(CustomMediaValue::MediaQueryList),
        }
    }
}
