use super::Parser;
use crate::{Parse, Spanned, ast::*, eat, error::PResult};

// https://developer.mozilla.org/en-US/docs/Web/CSS/@document
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for DocumentPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<DocumentPreludeMatcher>()?;
        let mut span = first.span().clone();

        let mut matchers = vec![first];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            matchers.push(input.parse()?);
        }
        debug_assert_eq!(comma_spans.len() + 1, matchers.len());

        if let Some(last) = matchers.last() {
            span.end = last.span().end;
        }
        Ok(DocumentPrelude {
            matchers,
            comma_spans,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for DocumentPreludeMatcher<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if let Ok(url) = input.try_parse(Url::parse) {
            Ok(DocumentPreludeMatcher::Url(url))
        } else {
            input.parse().map(DocumentPreludeMatcher::Function)
        }
    }
}
