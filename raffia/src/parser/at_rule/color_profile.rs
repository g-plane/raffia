use super::Parser;
use crate::{ast::*, error::PResult, Parse};

// https://www.w3.org/TR/css-color-5/#at-profile
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ColorProfilePrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.parse_dashed_ident()? {
            InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("device-cmyk") => {
                Ok(ColorProfilePrelude::DeviceCmyk(ident))
            }
            ident => Ok(ColorProfilePrelude::DashedIdent(ident)),
        }
    }
}
