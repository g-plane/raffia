use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    util,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_scroll_timeline_prelude(&mut self) -> PResult<InterpolableIdent<'s>> {
        match self.parse()? {
            // this should be recoverable
            InterpolableIdent::Literal(ident) if util::is_css_wide_keyword(&ident.name) => {
                Err(Error {
                    kind: ErrorKind::CSSWideKeywordDisallowed,
                    span: ident.span,
                })
            }
            ident => Ok(ident),
        }
    }
}
