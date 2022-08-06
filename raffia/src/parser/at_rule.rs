use super::Parser;
use crate::{
    ast::*,
    error::PResult,
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    Syntax,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_at_rule(&mut self) -> PResult<AtRule<'s>> {
        let at_keyword = expect!(self, AtKeyword);
        let block = match self.tokenizer.peek()? {
            Token::LBrace(..) | Token::Indent(..) => Some(self.parse_simple_block()?),
            _ => None,
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
            block,
            span,
        })
    }
}
