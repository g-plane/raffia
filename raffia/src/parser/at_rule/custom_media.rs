use super::Parser;
use crate::{
    ast::*,
    error::PResult,
    pos::{Span, Spanned},
    tokenizer::Token,
    Parse,
};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CustomMedia<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let name = input.parse_dashed_ident()?;
        let value = input.parse::<CustomMediaValue>()?;
        let span = Span {
            start: name.span().start,
            end: value.span().end,
        };
        Ok(CustomMedia { name, value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CustomMediaValue<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("true") => {
                input.parse().map(CustomMediaValue::True)
            }
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("false") => {
                input.parse().map(CustomMediaValue::False)
            }
            _ => input.parse().map(CustomMediaValue::MediaQueryList),
        }
    }
}
