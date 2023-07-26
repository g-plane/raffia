use super::Parser;
use crate::{ast::*, bump, error::PResult, peek, pos::Span, tokenizer::Token, util::PairedToken};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_tokens_in_parens(&mut self) -> PResult<TokenSeq<'s>> {
        let start = self.tokenizer.current_offset();
        let mut tokens = Vec::with_capacity(1);
        let mut pairs = Vec::with_capacity(1);
        loop {
            match &peek!(self).token {
                Token::LParen(..) => {
                    pairs.push(PairedToken::Paren);
                }
                Token::RParen(..) => {
                    if let Some(PairedToken::Paren) = pairs.pop() {
                    } else {
                        break;
                    }
                }
                Token::LBracket(..) => {
                    pairs.push(PairedToken::Bracket);
                }
                Token::RBracket(..) => {
                    if let Some(PairedToken::Bracket) = pairs.pop() {
                    } else {
                        break;
                    }
                }
                Token::LBrace(..) | Token::HashLBrace(..) => {
                    pairs.push(PairedToken::Brace);
                }
                Token::RBrace(..) => {
                    if let Some(PairedToken::Brace) = pairs.pop() {
                    } else {
                        break;
                    }
                }
                _ => {}
            }
            tokens.push(bump!(self));
        }
        let span = Span {
            start: tokens
                .first()
                .map(|token| token.span.start)
                .unwrap_or(start),
            end: if let Some(last) = tokens.last() {
                last.span.end
            } else {
                peek!(self).span.start
            },
        };
        Ok(TokenSeq { tokens, span })
    }
}
