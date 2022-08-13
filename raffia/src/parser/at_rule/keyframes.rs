use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    pos::{Span, Spanned},
    tokenizer::Token,
    util,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_keyframes_blocks(&mut self) -> PResult<SimpleBlock<'s>> {
        self.parse_simple_block_with(|parser| {
            let mut statements = vec![];
            loop {
                match parser.tokenizer.peek()? {
                    Token::RBrace(..) | Token::Dedent(..) | Token::Eof(..) => break,
                    _ => statements.push(Statement::KeyframeBlock(parser.parse_keyframe_block()?)),
                }
            }
            Ok(statements)
        })
    }

    fn parse_keyframe_block(&mut self) -> PResult<KeyframeBlock<'s>> {
        let (prelude, mut span) = self.parse_keyframe_selectors()?;
        let block = self.parse::<SimpleBlock>()?;
        span.end = block.span.end;
        Ok(KeyframeBlock {
            prelude,
            block,
            span,
        })
    }

    fn parse_keyframe_selectors(&mut self) -> PResult<(Vec<KeyframeSelector<'s>>, Span)> {
        let mut prelude = vec![];
        let mut span;
        match self.tokenizer.peek()? {
            Token::Percentage(..) => {
                let percentage = self.parse::<Percentage>()?;
                span = percentage.span.clone();
                prelude.push(KeyframeSelector::Percentage(percentage));
            }
            _ => {
                let ident = self.parse()?;
                match &ident {
                    InterpolableIdent::Literal(ident)
                        if !ident.name.eq_ignore_ascii_case("from")
                            && !ident.name.eq_ignore_ascii_case("to") =>
                    {
                        // this should be recoverable
                        return Err(Error {
                            kind: ErrorKind::UnknownKeyframeSelectorIdent,
                            span: ident.span.clone(),
                        });
                    }
                    _ => {}
                }
                span = ident.span().clone();
                prelude.push(KeyframeSelector::Ident(ident));
            }
        };
        while eat!(self, Comma).is_some() {
            match self.tokenizer.peek()? {
                Token::Percent(..) => prelude.push(KeyframeSelector::Percentage(self.parse()?)),
                _ => {
                    let ident = self.parse()?;
                    match &ident {
                        InterpolableIdent::Literal(ident)
                            if !ident.name.eq_ignore_ascii_case("from")
                                && !ident.name.eq_ignore_ascii_case("to") =>
                        {
                            // this should be recoverable
                            return Err(Error {
                                kind: ErrorKind::UnknownKeyframeSelectorIdent,
                                span: ident.span.clone(),
                            });
                        }
                        _ => {}
                    }
                    prelude.push(KeyframeSelector::Ident(ident))
                }
            };
        }

        if let Some(keyframe_selector) = prelude.last() {
            span.end = keyframe_selector.span().end;
        }
        Ok((prelude, span))
    }

    pub(super) fn parse_keyframes_prelude(&mut self) -> PResult<KeyframesName<'s>> {
        match self.tokenizer.peek()? {
            Token::Str(..) | Token::StrTemplate(..) => self.parse().map(KeyframesName::Str),
            _ => {
                let ident = self.parse()?;
                match &ident {
                    InterpolableIdent::Literal(ident)
                        if util::is_css_wide_keyword(&ident.name)
                            || ident.name.eq_ignore_ascii_case("default") =>
                    {
                        // this should be recoverable
                        return Err(Error {
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
