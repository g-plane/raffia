use super::Parser;
use crate::{
    ast::*,
    error::PResult,
    peek,
    pos::{Span, Spanned},
    tokenizer::Token,
    Parse,
};

// https://www.w3.org/TR/mediaqueries-5/#custom-mq
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
        match &peek!(input).token {
            Token::Ident(ident) => {
                let name = ident.name();
                if name.eq_ignore_ascii_case("true") {
                    input.parse().map(CustomMediaValue::True)
                } else if name.eq_ignore_ascii_case("false") {
                    input.parse().map(CustomMediaValue::False)
                } else {
                    input.parse().map(CustomMediaValue::MediaQueryList)
                }
            }
            _ => input.parse().map(CustomMediaValue::MediaQueryList),
        }
    }
}
