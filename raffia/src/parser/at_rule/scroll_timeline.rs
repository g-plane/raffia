use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    util,
};

// https://developer.mozilla.org/en-US/docs/Web/CSS/@scroll-timeline
impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_scroll_timeline_prelude(&mut self) -> PResult<InterpolableIdent<'s>> {
        let ident = self.parse()?;
        match &ident {
            InterpolableIdent::Literal(ident) if util::is_css_wide_keyword(&ident.name) => {
                self.recoverable_errors.push(Error {
                    kind: ErrorKind::CSSWideKeywordDisallowed,
                    span: ident.span.clone(),
                });
            }
            _ => {}
        }
        Ok(ident)
    }
}
