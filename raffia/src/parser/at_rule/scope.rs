use super::Parser;
use crate::{
    ast::*,
    bump,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::Span,
    tokenizer::{Token, TokenWithSpan},
    Parse,
};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ScopeEnd<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let to_span = match bump!(input) {
            TokenWithSpan {
                token: Token::Ident(ident),
                span,
            } if ident.name().eq_ignore_ascii_case("to") => span,
            TokenWithSpan { span, .. } => {
                return Err(Error {
                    kind: ErrorKind::ExpectScopeTo,
                    span,
                })
            }
        };

        let (_, lparen_span) = expect!(input, LParen);
        let selector = input.parse()?;
        let (_, Span { end, .. }) = expect!(input, RParen);

        let span = Span {
            start: to_span.start,
            end,
        };
        Ok(ScopeEnd {
            to_span,
            lparen_span,
            selector,
            span,
        })
    }
}

// https://drafts.csswg.org/css-cascade-6/#scope-syntax
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ScopePrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = if let Token::LParen(..) = peek!(input).token {
            Some(input.parse::<ScopeStart>()?)
        } else {
            None
        };
        let end = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("to") => {
                Some(input.parse::<ScopeEnd>()?)
            }
            _ => None,
        };

        match (start, end) {
            (Some(start), Some(end)) => {
                let span = Span {
                    start: start.span.start,
                    end: end.span.end,
                };
                Ok(ScopePrelude::Both(ScopeStartWithEnd { start, end, span }))
            }
            (Some(start), None) => Ok(ScopePrelude::StartOnly(start)),
            (None, Some(end)) => Ok(ScopePrelude::EndOnly(end)),
            (None, None) => {
                use crate::{token::LParen, tokenizer::TokenSymbol};
                let TokenWithSpan { token, span } = bump!(input);
                Err(Error {
                    kind: ErrorKind::Unexpected(LParen::symbol(), token.symbol()),
                    span,
                })
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ScopeStart<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, LParen);
        let selector = input.parse()?;
        let (_, Span { end, .. }) = expect!(input, RParen);

        Ok(ScopeStart {
            selector,
            span: Span { start, end },
        })
    }
}
