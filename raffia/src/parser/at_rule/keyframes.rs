use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    peek,
    pos::{Span, Spanned},
    tokenizer::Token,
    util, Parse,
};

// https://drafts.csswg.org/css-animations/#keyframes
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for KeyframeBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first_selector = input.parse::<KeyframeSelector>()?;
        let start = first_selector.span().start;

        let mut selectors = Vec::with_capacity(2);
        selectors.push(first_selector);
        while eat!(input, Comma).is_some() {
            selectors.push(input.parse()?);
        }

        let block = input.parse::<SimpleBlock>()?;
        let span = Span {
            start,
            end: block.span.end,
        };
        Ok(KeyframeBlock {
            selectors,
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
            _ => input.parse().map(KeyframesName::Str),
        }
    }
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_keyframes_blocks(&mut self) -> PResult<SimpleBlock<'s>> {
        self.parse_simple_block_with(|parser| {
            let mut statements = Vec::with_capacity(3);
            while let Token::Percentage(..)
            | Token::Ident(..)
            | Token::HashLBrace(..)
            | Token::AtLBraceVar(..) = &peek!(parser).token
            {
                statements.push(Statement::KeyframeBlock(parser.parse()?));
            }
            Ok(statements)
        })
    }
}
