use super::Parser;
use crate::{
    ast::*,
    error::PResult,
    expect,
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
mod scroll_timeline;
mod supports;

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for AtRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let at_keyword = expect!(input, AtKeyword);

        let at_rule_name = &at_keyword.ident.name;
        let prelude = if at_rule_name.eq_ignore_ascii_case("media") {
            input
                .try_parse(|parser| parser.parse())
                .ok()
                .map(AtRulePrelude::Media)
        } else if at_rule_name.eq_ignore_ascii_case("keyframes")
            || at_rule_name.eq_ignore_ascii_case("-webkit-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-moz-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-o-keyframes")
        {
            Some(AtRulePrelude::Keyframes(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("import") {
            Some(AtRulePrelude::Import(Box::new(input.parse()?)))
        } else if at_rule_name.eq_ignore_ascii_case("charset") {
            // https://drafts.csswg.org/css2/#charset%E2%91%A0
            Some(AtRulePrelude::Charset(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("supports") {
            Some(AtRulePrelude::Supports(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("layer") {
            input
                .try_parse(|parser| parser.parse())
                .map(AtRulePrelude::Layer)
                .ok()
        } else if at_rule_name.eq_ignore_ascii_case("container") {
            Some(AtRulePrelude::Container(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("page") {
            input
                .try_parse(|parser| parser.parse())
                .map(AtRulePrelude::Page)
                .ok()
        } else if at_rule_name.eq_ignore_ascii_case("namespace") {
            Some(AtRulePrelude::Namespace(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("color-profile") {
            Some(AtRulePrelude::ColorProfile(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("font-feature-values") {
            Some(AtRulePrelude::FontFeatureValues(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("font-palette-values") {
            // https://drafts.csswg.org/css-fonts/Overview.bs
            Some(AtRulePrelude::FontPaletteValues(
                input.parse_dashed_ident()?,
            ))
        } else if at_rule_name.eq_ignore_ascii_case("counter-style") {
            Some(AtRulePrelude::CounterStyle(
                input.parse_counter_style_prelude()?,
            ))
        } else if at_rule_name.eq_ignore_ascii_case("custom-media") {
            Some(AtRulePrelude::CustomMedia(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("scroll-timeline") {
            Some(AtRulePrelude::ScrollTimeline(
                input.parse_scroll_timeline_prelude()?,
            ))
        } else if at_rule_name.eq_ignore_ascii_case("position-fallback") {
            // https://tabatkins.github.io/specs/css-anchor-position/#fallback-rule
            Some(AtRulePrelude::PositionFallback(input.parse_dashed_ident()?))
        } else if at_rule_name.eq_ignore_ascii_case("nest") {
            // https://www.w3.org/TR/css-nesting-1/#at-nest
            Some(AtRulePrelude::Nest(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("property") {
            // https://drafts.css-houdini.org/css-properties-values-api/#at-property-rule
            Some(AtRulePrelude::Property(input.parse_dashed_ident()?))
        } else if at_rule_name.eq_ignore_ascii_case("document")
            || at_rule_name.eq_ignore_ascii_case("-moz-document")
        {
            Some(AtRulePrelude::Document(input.parse()?))
        } else if at_rule_name.eq_ignore_ascii_case("font-face")
            || at_rule_name.eq_ignore_ascii_case("viewport")
            || at_rule_name.eq_ignore_ascii_case("try")
            || at_rule_name.eq_ignore_ascii_case("stylistic")
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
        {
            None
        } else {
            input
                .parse_unknown_at_rule_prelude()?
                .map(AtRulePrelude::Unknown)
        };

        let block = if at_rule_name.eq_ignore_ascii_case("keyframes")
            || at_rule_name.eq_ignore_ascii_case("-webkit-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-moz-keyframes")
            || at_rule_name.eq_ignore_ascii_case("-o-keyframes")
        {
            Some(input.parse_keyframes_blocks()?)
        } else if at_rule_name.eq_ignore_ascii_case("media")
            || at_rule_name.eq_ignore_ascii_case("font-face")
            || at_rule_name.eq_ignore_ascii_case("supports")
            || at_rule_name.eq_ignore_ascii_case("layer")
            || at_rule_name.eq_ignore_ascii_case("container")
            || at_rule_name.eq_ignore_ascii_case("color-profile")
            || at_rule_name.eq_ignore_ascii_case("font-palette-values")
            || at_rule_name.eq_ignore_ascii_case("counter-style")
            || at_rule_name.eq_ignore_ascii_case("scroll-timeline")
            || at_rule_name.eq_ignore_ascii_case("position-fallback")
            || at_rule_name.eq_ignore_ascii_case("viewport")
            || at_rule_name.eq_ignore_ascii_case("nest")
            || at_rule_name.eq_ignore_ascii_case("property")
            || at_rule_name.eq_ignore_ascii_case("document")
            || at_rule_name.eq_ignore_ascii_case("-moz-document")
            || at_rule_name.eq_ignore_ascii_case("try")
            || at_rule_name.eq_ignore_ascii_case("font-feature-values")
            || at_rule_name.eq_ignore_ascii_case("stylistic")
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
        {
            input.parse().map(Some)?
        } else if at_rule_name.eq_ignore_ascii_case("import")
            || at_rule_name.eq_ignore_ascii_case("charset")
            || at_rule_name.eq_ignore_ascii_case("custom-media")
            || at_rule_name.eq_ignore_ascii_case("namespace")
        {
            None
        } else {
            match input.tokenizer.peek()? {
                Token::LBrace(..) | Token::Indent(..) => Some(input.parse()?),
                _ => None,
            }
        };

        let span = Span {
            start: at_keyword.span.start,
            end: match &block {
                Some(block) => block.span.end,
                None => {
                    if input.syntax == Syntax::Sass {
                        at_keyword.span.end
                    } else {
                        // next token should be semicolon, but it won't be consumed here
                        input.tokenizer.peek()?.span().end
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
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn parse_unknown_at_rule_prelude(&mut self) -> PResult<Option<TokenSeq<'s>>> {
        let mut tokens = vec![];
        loop {
            match self.tokenizer.peek()? {
                Token::LBrace(..)
                | Token::Indent(..)
                | Token::Dedent(..)
                | Token::Linebreak(..)
                | Token::Eof(..) => break,
                _ => tokens.push(self.tokenizer.bump()?),
            }
        }

        if let Some((first, last)) = tokens.first().zip(tokens.last()) {
            let span = Span {
                start: first.span().start,
                end: last.span().end,
            };
            Ok(Some(TokenSeq { tokens, span }))
        } else {
            Ok(None)
        }
    }
}
