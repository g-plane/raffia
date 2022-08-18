use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    util,
};

// https://www.w3.org/TR/css-counter-styles-3/#the-counter-style-rule
impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_counter_style_prelude(&mut self) -> PResult<InterpolableIdent<'s>> {
        let ident = self.parse()?;
        match &ident {
            InterpolableIdent::Literal(ident)
                if util::is_css_wide_keyword(&ident.name)
                    || ident.name.eq_ignore_ascii_case("none") =>
            {
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
