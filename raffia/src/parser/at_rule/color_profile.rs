use super::Parser;
use crate::{ast::*, error::PResult};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_color_profile_prelude(&mut self) -> PResult<ColorProfilePrelude<'s>> {
        match self.parse_dashed_ident()? {
            InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("device-cmyk") => {
                Ok(ColorProfilePrelude::DeviceCmyk(ident))
            }
            ident => Ok(ColorProfilePrelude::DashedIdent(ident)),
        }
    }
}
