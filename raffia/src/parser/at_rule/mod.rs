use super::{state::ParserState, Parser};
use crate::{
    ast::*,
    bump,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::Token,
    Parse, Syntax,
};

mod color_profile;
mod container;
mod counter_style;
mod custom_media;
mod document;
mod font_feature_values;
mod import;
mod keyframes;
mod layer;
mod media;
mod namespace;
mod page;
mod scope;
mod supports;

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for AtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (at_keyword, at_keyword_span) = expect!(input, AtKeyword);

        let at_rule_name = at_keyword.ident.name();
        let (prelude, block, end) = if at_rule_name.eq_ignore_ascii_case("media") {
            let prelude = input
                .try_parse(MediaQueryList::parse)
                .ok()
                .map(AtRulePrelude::Media);
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("keyframes")
            || at_rule_name.eq_ignore_ascii_case("-webkit-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-moz-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-ms-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-o-keyframes")
        {
            let prelude = AtRulePrelude::Keyframes(input.parse()?);
            let block = input
                .with_state(ParserState {
                    in_keyframes_at_rule: true,
                    ..input.state.clone()
                })
                .parse::<SimpleBlock>()?;
            let end = block.span.end;
            (Some(prelude), Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("import") {
            let (end, prelude) = match input.syntax {
                Syntax::Css => {
                    let prelude = input.parse::<ImportPrelude>()?;
                    (prelude.span.end, AtRulePrelude::Import(Box::new(prelude)))
                }
                Syntax::Scss | Syntax::Sass => {
                    if let Ok(prelude) = input.try_parse(ImportPrelude::parse) {
                        (prelude.span.end, AtRulePrelude::Import(Box::new(prelude)))
                    } else {
                        let prelude = input.parse::<SassImportPrelude>()?;
                        (prelude.span.end, AtRulePrelude::SassImport(prelude))
                    }
                }
                Syntax::Less => {
                    if let Ok(prelude) = input.try_parse(ImportPrelude::parse) {
                        (prelude.span.end, AtRulePrelude::Import(Box::new(prelude)))
                    } else {
                        let prelude = input.parse::<LessImportPrelude>()?;
                        (
                            prelude.span.end,
                            AtRulePrelude::LessImport(Box::new(prelude)),
                        )
                    }
                }
            };
            (Some(prelude), None, end)
        } else if at_rule_name.eq_ignore_ascii_case("charset") {
            // https://drafts.csswg.org/css2/#charset%E2%91%A0
            let prelude = input.parse::<Str>()?;
            let end = prelude.span.end;
            (Some(AtRulePrelude::Charset(prelude)), None, end)
        } else if at_rule_name.eq_ignore_ascii_case("font-face") {
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (None, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("supports") {
            let prelude = Some(AtRulePrelude::Supports(input.parse()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("layer") {
            let prelude = input
                .try_parse(LayerName::parse)
                .map(AtRulePrelude::Layer)
                .ok();
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("container") {
            let prelude = Some(AtRulePrelude::Container(input.parse()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("page") {
            let prelude = input
                .try_parse(PageSelectorList::parse)
                .map(AtRulePrelude::Page)
                .ok();
            let block = input.try_parse(SimpleBlock::parse).ok();
            let end = block
                .as_ref()
                .map(|block| block.span.end)
                .or_else(|| prelude.as_ref().map(|prelude| prelude.span().end))
                .unwrap_or(at_keyword_span.end);
            (prelude, block, end)
        } else if at_rule_name.eq_ignore_ascii_case("namespace") {
            let namespace = input.parse::<NamespacePrelude>()?;
            let end = namespace.span.end;
            (Some(AtRulePrelude::Namespace(namespace)), None, end)
        } else if at_rule_name.eq_ignore_ascii_case("color-profile") {
            let prelude = Some(AtRulePrelude::ColorProfile(input.parse()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("font-feature-values") {
            let prelude = Some(AtRulePrelude::FontFeatureValues(input.parse()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("font-palette-values") {
            // https://drafts.csswg.org/css-fonts/Overview.bs
            let prelude = Some(AtRulePrelude::FontPaletteValues(
                input.parse_dashed_ident()?,
            ));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("counter-style") {
            let prelude = Some(AtRulePrelude::CounterStyle(
                input.parse_counter_style_prelude()?,
            ));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("custom-media") {
            let custom_media = input.parse::<CustomMedia>()?;
            let end = custom_media.span.end;
            (Some(AtRulePrelude::CustomMedia(custom_media)), None, end)
        } else if at_rule_name.eq_ignore_ascii_case("position-fallback") {
            // https://tabatkins.github.io/specs/css-anchor-position/#fallback-rule
            let prelude = Some(AtRulePrelude::PositionFallback(input.parse_dashed_ident()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("nest") {
            // https://www.w3.org/TR/css-nesting-1/#at-nest
            let prelude = Some(AtRulePrelude::Nest(input.parse()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("property") {
            // https://drafts.css-houdini.org/css-properties-values-api/#at-property-rule
            let prelude = Some(AtRulePrelude::Property(input.parse_dashed_ident()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("scope") {
            let prelude = if let Token::LParen(..) | Token::Ident(..) = peek!(input).token {
                Some(AtRulePrelude::Scope(Box::new(input.parse()?)))
            } else {
                None
            };
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("document")
            || at_rule_name.eq_ignore_ascii_case("-moz-document")
        {
            let prelude = Some(AtRulePrelude::Document(input.parse()?));
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (prelude, Some(block), end)
        } else if at_rule_name.eq_ignore_ascii_case("stylistic")
            || at_rule_name.eq_ignore_ascii_case("historical-forms")
            || at_rule_name.eq_ignore_ascii_case("styleset")
            || at_rule_name.eq_ignore_ascii_case("character-variant")
            || at_rule_name.eq_ignore_ascii_case("swash")
            || at_rule_name.eq_ignore_ascii_case("ornaments")
            || at_rule_name.eq_ignore_ascii_case("annotation")
            || at_rule_name.eq_ignore_ascii_case("top-left-corner")
            || at_rule_name.eq_ignore_ascii_case("top-left")
            || at_rule_name.eq_ignore_ascii_case("top-center")
            || at_rule_name.eq_ignore_ascii_case("top-right")
            || at_rule_name.eq_ignore_ascii_case("top-right-corner")
            || at_rule_name.eq_ignore_ascii_case("bottom-left-corner")
            || at_rule_name.eq_ignore_ascii_case("bottom-left")
            || at_rule_name.eq_ignore_ascii_case("bottom-center")
            || at_rule_name.eq_ignore_ascii_case("bottom-right")
            || at_rule_name.eq_ignore_ascii_case("bottom-right-corner")
            || at_rule_name.eq_ignore_ascii_case("left-top")
            || at_rule_name.eq_ignore_ascii_case("left-middle")
            || at_rule_name.eq_ignore_ascii_case("left-bottom")
            || at_rule_name.eq_ignore_ascii_case("right-top")
            || at_rule_name.eq_ignore_ascii_case("right-middle")
            || at_rule_name.eq_ignore_ascii_case("right-bottom")
            || at_rule_name.eq_ignore_ascii_case("viewport")
            || at_rule_name.eq_ignore_ascii_case("try")
            || at_rule_name.eq_ignore_ascii_case("starting-style")
        {
            let block = input.parse::<SimpleBlock>()?;
            let end = block.span.end;
            (None, Some(block), end)
        } else if at_rule_name == "plugin" && input.syntax == Syntax::Less {
            let prelude = input.parse::<LessPlugin>()?;
            let end = prelude.span.end;
            (
                Some(AtRulePrelude::LessPlugin(Box::new(prelude))),
                None,
                end,
            )
        } else if matches!(input.syntax, Syntax::Scss | Syntax::Sass) {
            use super::state::{
                SASS_CTX_ALLOW_DIV, SASS_CTX_ALLOW_KEYFRAME_BLOCK, SASS_CTX_IN_FUNCTION,
            };
            match &*at_rule_name {
                "each" => {
                    let prelude = input.parse()?;
                    let block = input.parse::<SimpleBlock>()?;
                    let end = block.span.end;
                    (
                        Some(AtRulePrelude::SassEach(Box::new(prelude))),
                        Some(block),
                        end,
                    )
                }
                "while" => {
                    let prelude = input.parse()?;
                    let block = input.parse::<SimpleBlock>()?;
                    let end = block.span.end;
                    (
                        Some(AtRulePrelude::SassExpr(Box::new(prelude))),
                        Some(block),
                        end,
                    )
                }
                "for" => {
                    let prelude = input.parse()?;
                    let block = input.parse::<SimpleBlock>()?;
                    let end = block.span.end;
                    (
                        Some(AtRulePrelude::SassFor(Box::new(prelude))),
                        Some(block),
                        end,
                    )
                }
                "mixin" => {
                    let prelude = input.parse()?;
                    let block = input
                        .with_state(ParserState {
                            sass_ctx: input.state.sass_ctx | SASS_CTX_ALLOW_KEYFRAME_BLOCK,
                            ..input.state.clone()
                        })
                        .parse::<SimpleBlock>()?;
                    let end = block.span.end;
                    (
                        Some(AtRulePrelude::SassMixin(Box::new(prelude))),
                        Some(block),
                        end,
                    )
                }
                "include" => {
                    let prelude = input.parse::<SassInclude>()?;
                    let block =
                        if matches!(peek!(input).token, Token::LBrace(..) | Token::Indent(..)) {
                            Some(
                                input
                                    .with_state(ParserState {
                                        sass_ctx: input.state.sass_ctx
                                            | SASS_CTX_ALLOW_KEYFRAME_BLOCK,
                                        ..input.state.clone()
                                    })
                                    .parse::<SimpleBlock>()?,
                            )
                        } else {
                            None
                        };
                    let end = block
                        .as_ref()
                        .map(|block| block.span.end)
                        .unwrap_or(prelude.span.end);
                    (
                        Some(AtRulePrelude::SassInclude(Box::new(prelude))),
                        block,
                        end,
                    )
                }
                "content" => {
                    if matches!(peek!(input).token, Token::LParen(..)) {
                        let prelude = input.parse::<SassContent>()?;
                        let end = prelude.span.end;
                        (Some(AtRulePrelude::SassContent(prelude)), None, end)
                    } else {
                        (None, None, input.tokenizer.current_offset())
                    }
                }
                "use" => {
                    let prelude = input.parse::<SassUse>()?;
                    let end = prelude.span.end;
                    (Some(AtRulePrelude::SassUse(Box::new(prelude))), None, end)
                }
                "function" => {
                    let prelude = input.parse::<SassFunction>()?;
                    let block = input
                        .with_state(ParserState {
                            sass_ctx: input.state.sass_ctx | SASS_CTX_IN_FUNCTION,
                            ..input.state.clone()
                        })
                        .parse::<SimpleBlock>()?;
                    let end = block.span.end;
                    (
                        Some(AtRulePrelude::SassFunction(Box::new(prelude))),
                        Some(block),
                        end,
                    )
                }
                "return" => {
                    let expr = input
                        .with_state(ParserState {
                            sass_ctx: input.state.sass_ctx | SASS_CTX_ALLOW_DIV,
                            ..input.state.clone()
                        })
                        .parse_maybe_sass_list(/* allow_comma */ true)?;
                    let end = expr.span().end;
                    if input.state.sass_ctx & SASS_CTX_IN_FUNCTION == 0 {
                        input.recoverable_errors.push(Error {
                            kind: ErrorKind::ReturnOutsideFunction,
                            span: Span {
                                start: at_keyword_span.start,
                                end,
                            },
                        });
                    }
                    (Some(AtRulePrelude::SassExpr(Box::new(expr))), None, end)
                }
                "extend" => {
                    let prelude = input.parse::<SassExtend>()?;
                    let end = prelude.span.end;
                    (
                        Some(AtRulePrelude::SassExtend(Box::new(prelude))),
                        None,
                        end,
                    )
                }
                "warn" | "error" | "debug" => {
                    let expr = input.parse_maybe_sass_list(/* allow_comma */ true)?;
                    let end = expr.span().end;
                    (Some(AtRulePrelude::SassExpr(Box::new(expr))), None, end)
                }
                "forward" => {
                    let prelude = input.parse::<SassForward>()?;
                    let end = prelude.span.end;
                    (
                        Some(AtRulePrelude::SassForward(Box::new(prelude))),
                        None,
                        end,
                    )
                }
                "at-root" => {
                    let prelude =
                        if !matches!(peek!(input).token, Token::LBrace(..) | Token::Indent(..)) {
                            Some(AtRulePrelude::SassAtRoot(input.parse()?))
                        } else {
                            None
                        };
                    let block = input.parse::<SimpleBlock>()?;
                    let end = block.span.end;
                    (prelude, Some(block), end)
                }
                _ => {
                    let (prelude, block, end) = input.parse_unknown_at_rule()?;
                    (
                        prelude.map(AtRulePrelude::Unknown),
                        block,
                        end.unwrap_or(at_keyword_span.end),
                    )
                }
            }
        } else {
            let (prelude, block, end) = input.parse_unknown_at_rule()?;
            (
                prelude.map(AtRulePrelude::Unknown),
                block,
                end.unwrap_or(at_keyword_span.end),
            )
        };

        let span = Span {
            start: at_keyword_span.start,
            end,
        };
        Ok(AtRule {
            // We don't use `Ident::from_token` here because `at_rule_name` is already
            // type of `Cow<str>`, avoiding creating it again.
            name: Ident {
                name: at_rule_name,
                raw: at_keyword.ident.raw,
                span: Span {
                    start: at_keyword_span.start + 1,
                    end: at_keyword_span.end,
                },
            },
            prelude,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_unknown_at_rule(
        &mut self,
    ) -> PResult<(
        Option<UnknownAtRulePrelude<'s>>,
        Option<SimpleBlock<'s>>,
        Option<usize>,
    )> {
        let prelude = self.parse_unknown_at_rule_prelude()?;
        let block = match &peek!(self).token {
            Token::LBrace(..) | Token::Indent(..) => Some(self.parse::<SimpleBlock>()?),
            _ => None,
        };
        let end = block
            .as_ref()
            .map(|block| block.span.end)
            .or_else(|| prelude.as_ref().map(|prelude| prelude.span().end));
        Ok((prelude, block, end))
    }

    fn parse_unknown_at_rule_prelude(&mut self) -> PResult<Option<UnknownAtRulePrelude<'s>>> {
        if let Ok(value) = self.try_parse(|parser| {
            match parser.syntax {
                Syntax::Css => parser.parse(),
                Syntax::Scss | Syntax::Sass => {
                    parser.parse_maybe_sass_list(/* allow_comma */ true)
                }
                Syntax::Less => parser.parse_maybe_less_list(/* allow_comma */ true),
            }
        }) {
            return Ok(Some(UnknownAtRulePrelude::ComponentValue(value)));
        }

        let mut tokens = vec![];
        loop {
            match &peek!(self).token {
                Token::LBrace(..)
                | Token::Semicolon(..)
                | Token::Indent(..)
                | Token::Dedent(..)
                | Token::Linebreak(..)
                | Token::Eof(..) => break,
                _ => tokens.push(bump!(self)),
            }
        }

        if let Some((first, last)) = tokens.first().zip(tokens.last()) {
            let span = Span {
                start: first.span().start,
                end: last.span().end,
            };
            Ok(Some(UnknownAtRulePrelude::TokenSeq(TokenSeq {
                tokens,
                span,
            })))
        } else {
            Ok(None)
        }
    }
}
