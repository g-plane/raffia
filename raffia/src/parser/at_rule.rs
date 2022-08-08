use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    util, Syntax,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_at_rule(&mut self) -> PResult<AtRule<'s>> {
        let at_keyword = expect!(self, AtKeyword);

        let at_rule_name = at_keyword.ident.name.clone();
        let prelude = if at_rule_name.eq_ignore_ascii_case("keyframes") {
            Some(AtRulePrelude::Keyframes(self.parse_keyframes_prelude()?))
        } else {
            None
        };

        let block = if at_rule_name.eq_ignore_ascii_case("keyframes") {
            Some(self.parse_keyframes_blocks()?)
        } else {
            match self.tokenizer.peek()? {
                Token::LBrace(..) | Token::Indent(..) => Some(self.parse_simple_block()?),
                _ => None,
            }
        };

        let span = Span {
            start: at_keyword.span.start,
            end: match &block {
                Some(block) => block.span.end,
                None => {
                    if self.syntax == Syntax::Sass {
                        at_keyword.span.end
                    } else {
                        // next token should be semicolon, but it won't be consumed here
                        self.tokenizer.peek()?.span().end
                    }
                }
            },
        };
        Ok(AtRule {
            name: at_keyword.ident.into(),
            prelude,
            block,
            span,
        })
    }

    fn parse_keyframes_blocks(&mut self) -> PResult<SimpleBlock<'s>> {
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
        let block = self.parse_simple_block()?;
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
                let percentage = self.parse_percentage()?;
                span = percentage.span.clone();
                prelude.push(KeyframeSelector::Percentage(percentage));
            }
            _ => {
                let ident = self.parse_interpolable_ident()?;
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
                Token::Percent(..) => {
                    prelude.push(KeyframeSelector::Percentage(self.parse_percentage()?))
                }
                _ => {
                    let ident = self.parse_interpolable_ident()?;
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

    fn parse_keyframes_prelude(&mut self) -> PResult<KeyframesName<'s>> {
        match self.tokenizer.peek()? {
            Token::Str(..) | Token::StrTemplate(..) => {
                self.parse_interpolable_str().map(KeyframesName::Str)
            }
            _ => {
                let ident = self.parse_interpolable_ident()?;
                match &ident {
                    InterpolableIdent::Literal(ident)
                        if util::is_css_wide_keyword(&ident.name)
                            || ident.name.eq_ignore_ascii_case("default") =>
                    {
                        // this should be recoverable
                        return Err(Error {
                            kind: ErrorKind::InvalidCSSCustomIdent,
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
