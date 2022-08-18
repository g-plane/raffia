use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    pos::{Span, Spanned},
    tokenizer::Token,
    util, Parse,
};

// https://drafts.csswg.org/css-animations/#keyframes
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for KeyframeBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (prelude, mut span) = input.parse_keyframe_selectors()?;
        let block = input.parse::<SimpleBlock>()?;
        span.end = block.span.end;
        Ok(KeyframeBlock {
            prelude,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for KeyframeSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
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
        match input.tokenizer.peek()? {
            Token::Str(..) | Token::StrTemplate(..) => input.parse().map(KeyframesName::Str),
            _ => {
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
        }
    }
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_keyframes_blocks(&mut self) -> PResult<SimpleBlock<'s>> {
        self.parse_simple_block_with(|parser| {
            let mut statements = vec![];
            loop {
                match parser.tokenizer.peek()? {
                    Token::RBrace(..) | Token::Dedent(..) | Token::Eof(..) => break,
                    _ => statements.push(Statement::KeyframeBlock(parser.parse()?)),
                }
            }
            Ok(statements)
        })
    }

    fn parse_keyframe_selectors(&mut self) -> PResult<(Vec<KeyframeSelector<'s>>, Span)> {
        let first = self.parse::<KeyframeSelector>()?;
        let mut span = first.span().clone();

        let mut prelude = vec![first];
        while eat!(self, Comma).is_some() {
            prelude.push(self.parse()?);
        }

        if let Some(keyframe_selector) = prelude.last() {
            span.end = keyframe_selector.span().end;
        }
        Ok((prelude, span))
    }
}
