use super::Parser;
use crate::{
    Parse,
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    parser::state::ParserState,
    peek,
    pos::{Span, Spanned},
    tokenizer::Token,
    util,
};

// https://drafts.csswg.org/css-animations/#keyframes
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for KeyframeBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first_selector = input.parse::<KeyframeSelector>()?;
        let start = first_selector.span().start;

        let mut selectors = Vec::with_capacity(2);
        selectors.push(first_selector);
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            selectors.push(input.parse()?);
        }
        debug_assert_eq!(comma_spans.len() + 1, selectors.len());

        let block = input
            .with_state(ParserState {
                in_keyframes_at_rule: false,
                ..input.state.clone()
            })
            .parse::<SimpleBlock>()?;

        let span = Span {
            start,
            end: block.span.end,
        };
        Ok(KeyframeBlock {
            selectors,
            comma_spans,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for KeyframeSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match &peek!(input).token {
            Token::Percentage(..) => Ok(KeyframeSelector::Percentage(input.parse()?)),
            _ => {
                let ident = input.parse()?;
                match &ident {
                    InterpolableIdent::Literal(ident)
                        if !ident.name.eq_ignore_ascii_case("from")
                            && !ident.name.eq_ignore_ascii_case("to") =>
                    {
                        input.recoverable_errors.push(Error {
                            kind: ErrorKind::UnknownKeyframeSelectorIdent,
                            span: ident.span.clone(),
                        });
                    }
                    _ => {}
                }
                Ok(KeyframeSelector::Ident(ident))
            }
        }
    }
}

// https://drafts.csswg.org/css-animations/#keyframes
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for KeyframesName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match &peek!(input).token {
            Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                let ident = input.parse()?;
                match &ident {
                    InterpolableIdent::Literal(ident)
                        if util::is_css_wide_keyword(&ident.name)
                            || ident.name.eq_ignore_ascii_case("default") =>
                    {
                        input.recoverable_errors.push(Error {
                            kind: ErrorKind::CSSWideKeywordDisallowed,
                            span: ident.span.clone(),
                        });
                    }
                    _ => {}
                }
                Ok(KeyframesName::Ident(ident))
            }
            Token::AtKeyword(..) => input.parse().map(KeyframesName::LessVariable),
            Token::Tilde(..) => input.parse().map(KeyframesName::LessEscapedStr),
            _ => input.parse().map(KeyframesName::Str),
        }
    }
}
