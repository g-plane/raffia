use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_position_fallback_prelude(&mut self) -> PResult<InterpolableIdent<'s>> {
        match self.parse_interpolable_ident()? {
            // this should be recoverable
            InterpolableIdent::Literal(ident) if !ident.name.starts_with("--") => Err(Error {
                kind: ErrorKind::ExpectDashedIdent,
                span: ident.span,
            }),
            ident => Ok(ident),
        }
    }
}
