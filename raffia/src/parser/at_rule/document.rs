use super::Parser;
use crate::{ast::*, eat, error::PResult, Parse, Spanned};

// https://developer.mozilla.org/en-US/docs/Web/CSS/@document
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for DocumentPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<DocumentPreludeMatcher>()?;
        let mut span = first.span().clone();

        let mut matchers = vec![first];
        while eat!(input, Comma).is_some() {
            matchers.push(input.parse()?);
        }
        if let Some(last) = matchers.last() {
            span.end = last.span().end;
        }
        Ok(DocumentPrelude { matchers, span })
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
