use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    Parse,
};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ImportPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let href = match input.tokenizer.peek()? {
            Token::UrlPrefix(..) => input.parse().map(ImportPreludeHref::Url)?,
            _ => input.parse().map(ImportPreludeHref::Str)?,
        };
        let mut span = href.span().clone();

        let layer = match input.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("layer") => {
                let ident = input.parse::<Ident>()?;
                let layer = match input.tokenizer.peek()? {
                    Token::LParen(l_paren) if l_paren.span.start == ident.span.end => {
                        expect!(input, LParen);
                        let args = match input.tokenizer.peek()? {
                            Token::RParen(..) => vec![],
                            _ => vec![input.parse().map(ComponentValue::LayerName)?],
                        };
                        let r_paren = expect!(input, RParen);
                        let span = Span {
                            start: ident.span.start,
                            end: r_paren.span.end,
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
            let ident = expect!(parser, Ident);
            if !ident.name.eq_ignore_ascii_case("supports") {
                return Err(Error {
                    kind: ErrorKind::TryParseError,
                    span: ident.span,
                });
            }

            let l_paren = expect!(parser, LParen);
            parser.assert_no_ws_or_comment(&ident.span, &l_paren.span)?;

            let supports =
                if let Some(supports_condition) = parser.try_parse(|parser| parser.parse()) {
                    Ok(ImportPreludeSupports::SupportsCondition(supports_condition))
                } else {
                    parser.parse().map(ImportPreludeSupports::Declaration)
                };
            expect!(parser, RParen);
            supports
        });
        if let Some(supports) = &supports {
            span.end = supports.span().end;
        }

        let media = match input.tokenizer.peek()? {
            Token::Semicolon(..) => None,
            _ => {
                let media = input.parse::<MediqQueryList>()?;
                span.end = media.span.end;
                Some(media)
            }
        };

        Ok(ImportPrelude {
            href,
            layer,
            supports,
            media,
            span,
        })
    }
}