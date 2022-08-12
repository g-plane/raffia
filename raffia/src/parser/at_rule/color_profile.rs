use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_color_profile_prelude(&mut self) -> PResult<ColorProfilePrelude<'s>> {
        match self.parse_interpolable_ident()? {
            InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("device-cmyk") => {
                Ok(ColorProfilePrelude::DeviceCmyk(ident))
            }
            // this should be recoverable
            InterpolableIdent::Literal(ident) if !ident.name.starts_with("--") => Err(Error {
                kind: ErrorKind::ExpectDashedIdent,
                span: ident.span,
            }),
            ident => Ok(ColorProfilePrelude::DashedIdent(ident)),
        }
    }
}
