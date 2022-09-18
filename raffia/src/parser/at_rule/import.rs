use super::Parser;
use crate::{
    ast::*,
    bump,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    Parse,
};

// https://www.w3.org/TR/css-cascade-5/#at-import
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ImportPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let href = match &peek!(input).token {
            Token::Str(..) | Token::StrTemplate(..) => input.parse().map(ImportPreludeHref::Str)?,
            _ => input.parse().map(ImportPreludeHref::Url)?,
        };
        let mut span = href.span().clone();

        let layer = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("layer") => {
                let ident = input.parse::<Ident>()?;
                let layer = match peek!(input) {
                    TokenWithSpan {
                        token: Token::LParen(..),
                        span,
                    } if span.start == ident.span.end => {
                        bump!(input);
                        let args = vec![input.parse().map(ComponentValue::LayerName)?];
                        let end = expect!(input, RParen).1.end;
                        let span = Span {
                            start: ident.span.start,
                            end,
                        };
                        ImportPreludeLayer::WithName(Function {
                            name: InterpolableIdent::Literal(ident),
                            args,
                            span,
                        })
                    }
                    _ => ImportPreludeLayer::Empty(ident),
                };
                span.end = layer.span().end;
                Some(layer)
            }
            _ => None,
        };

        let supports = input.try_parse(|parser| {
            let (ident, span) = expect!(parser, Ident);
            if !ident.name().eq_ignore_ascii_case("supports") {
                return Err(Error {
                    kind: ErrorKind::TryParseError,
                    span,
                });
            }

            expect_without_ws_or_comments!(parser, LParen);

            let supports = if let Ok(supports_condition) = parser.try_parse(|parser| parser.parse())
            {
                Ok(ImportPreludeSupports::SupportsCondition(supports_condition))
            } else {
                parser.parse().map(ImportPreludeSupports::Declaration)
            };
            expect!(parser, RParen);
            supports
        });
        if let Ok(supports) = &supports {
            span.end = supports.span().end;
        }

        let media = match &peek!(input).token {
            Token::Semicolon(..) => None,
            _ => {
                let media = input.parse::<MediaQueryList>()?;
                span.end = media.span.end;
                Some(media)
            }
        };

        Ok(ImportPrelude {
            href,
            layer,
            supports: supports.ok(),
            media,
            span,
        })
    }
}
