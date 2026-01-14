use super::Parser;
use crate::{
    Parse,
    ast::*,
    eat,
    error::PResult,
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util,
};

// https://www.w3.org/TR/css-page-3/#syntax-page-selector
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for PageSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let mut name = None;
        let mut pseudo = vec![];
        let start;
        let mut end;

        if let Token::Colon(..) = &peek!(input).token {
            let first = input.parse::<PseudoPage>()?;
            start = first.span.start;
            end = first.span.end;
            pseudo.push(first);
        } else {
            let ident = input.parse::<InterpolableIdent>()?;
            let ident_span = ident.span();
            start = ident_span.start;
            end = ident_span.end;
            name = Some(ident)
        }

        loop {
            match peek!(input) {
                TokenWithSpan {
                    token: Token::Colon(..),
                    span,
                } if span.start == end => {
                    let item = input.parse::<PseudoPage>()?;
                    end = item.span.end;
                    pseudo.push(item);
                }
                _ => break,
            }
        }

        Ok(PageSelector {
            name,
            pseudo,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for PageSelectorList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<PageSelector>()?;
        let mut span = first.span.clone();

        let mut selectors = vec![first];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            selectors.push(input.parse()?);
        }

        if let Some(last) = selectors.last() {
            span.end = last.span.end;
        }
        Ok(PageSelectorList {
            selectors,
            comma_spans,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for PseudoPage<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, colon_span) = expect!(input, Colon);
        let name = input.parse::<InterpolableIdent>()?;

        let name_span = name.span();
        util::assert_no_ws_or_comment(&colon_span, name_span)?;

        let span = Span {
            start: colon_span.start,
            end: name_span.end,
        };
        Ok(PseudoPage { name, span })
    }
}
