use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(in crate::parser) fn parse_import_prelude(&mut self) -> PResult<ImportPrelude<'s>> {
        let href = match self.tokenizer.peek()? {
            Token::UrlPrefix(..) => self.parse_url().map(ImportPreludeHref::Url)?,
            _ => self.parse_interpolable_str().map(ImportPreludeHref::Str)?,
        };
        let mut span = href.span().clone();

        let layer = match self.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("layer") => {
                let ident = self.parse_ident()?;
                let layer = match self.tokenizer.peek()? {
                    Token::LParen(l_paren) if l_paren.span.start == ident.span.end => {
                        expect!(self, LParen);
                        let args = match self.tokenizer.peek()? {
                            Token::RParen(..) => vec![],
                            _ => vec![self.parse_layer_name().map(ComponentValue::LayerName)?],
                        };
                        let r_paren = expect!(self, RParen);
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

        let supports = self.try_parse(|parser| {
            let ident = expect!(parser, Ident);
            if !ident.name.eq_ignore_ascii_case("supports") {
                return Err(Error {
                    kind: ErrorKind::TryParseError,
                    span: ident.span,
                });
            }

            let l_paren = expect!(parser, LParen);
            parser.assert_no_ws_or_comment(&ident.span, &l_paren.span)?;

            let supports = if let Some(supports_condition) =
                parser.try_parse(|parser| parser.parse_supports_condition())
            {
                Ok(ImportPreludeSupports::SupportsCondition(supports_condition))
            } else {
                parser
                    .parse_declaration()
                    .map(ImportPreludeSupports::Declaration)
            };
            expect!(parser, RParen);
            supports
        });
        if let Some(supports) = &supports {
            span.end = supports.span().end;
        }

        let media = match self.tokenizer.peek()? {
            Token::Semicolon(..) => None,
            _ => {
                let media = self.parse_media_query_list()?;
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
