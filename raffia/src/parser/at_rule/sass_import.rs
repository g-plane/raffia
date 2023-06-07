use super::Parser;
use crate::{ast::*, eat, error::PResult, pos::Span, Parse};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SassImportPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<Str>()?;
        let mut span = first.span.clone();

        let mut paths = vec![first];
        while eat!(input, Comma).is_some() {
            paths.push(input.parse()?);
        }

        if let Some(Str {
            span: Span { end, .. },
            ..
        }) = paths.last()
        {
            span.end = *end;
        }
        Ok(SassImportPrelude { paths, span })
    }
}
