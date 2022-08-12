use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    util,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_counter_style_prelude(&mut self) -> PResult<InterpolableIdent<'s>> {
        match self.parse_interpolable_ident()? {
            // this should be recoverable
            InterpolableIdent::Literal(ident)
                if util::is_css_wide_keyword(&ident.name)
                    || ident.name.eq_ignore_ascii_case("none") =>
            {
                Err(Error {
                    kind: ErrorKind::CSSWideKeywordDisallowed,
                    span: ident.span,
                })
            }
            ident => Ok(ident),
        }
    }
}
