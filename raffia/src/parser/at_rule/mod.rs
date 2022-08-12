use super::Parser;
use crate::{
    ast::*,
    error::PResult,
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    Syntax,
};

mod custom_media;
mod keyframes;
mod layer;
mod media;
mod position_fallback;
mod supports;

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_at_rule(&mut self) -> PResult<AtRule<'s>> {
        let at_keyword = expect!(self, AtKeyword);

        let at_rule_name = &at_keyword.ident.name;
        #[allow(clippy::if_same_then_else)]
        let prelude = if at_rule_name.eq_ignore_ascii_case("keyframes") {
            Some(AtRulePrelude::Keyframes(self.parse_keyframes_prelude()?))
        } else if at_rule_name.eq_ignore_ascii_case("media") {
            Some(AtRulePrelude::Media(self.parse_media_query_list()?))
        } else if at_rule_name.eq_ignore_ascii_case("charset") {
            Some(AtRulePrelude::Charset(self.parse_str()?))
        } else if at_rule_name.eq_ignore_ascii_case("supports") {
            Some(AtRulePrelude::Supports(self.parse_supports_condition()?))
        } else if at_rule_name.eq_ignore_ascii_case("layer") {
            match self.tokenizer.peek()? {
                Token::Ident(..) => Some(AtRulePrelude::Layer(self.parse_layer_name()?)),
                Token::HashLBrace(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    Some(AtRulePrelude::Layer(self.parse_layer_name()?))
                }
                Token::AtLBraceVar(..) if self.syntax == Syntax::Less => {
                    Some(AtRulePrelude::Layer(self.parse_layer_name()?))
                }
                _ => None,
            }
        } else if at_rule_name.eq_ignore_ascii_case("custom-media") {
            Some(AtRulePrelude::CustomMedia(self.parse_custom_media()?))
        } else if at_rule_name.eq_ignore_ascii_case("position-fallback") {
            Some(AtRulePrelude::PositionFallback(
                self.parse_position_fallback_prelude()?,
            ))
        } else if at_rule_name.eq_ignore_ascii_case("try") {
            None
        } else {
            // todo: allow any tokens
            None
        };

        let block = if at_rule_name.eq_ignore_ascii_case("keyframes") {
            Some(self.parse_keyframes_blocks()?)
        } else if at_rule_name.eq_ignore_ascii_case("media")
            || at_rule_name.eq_ignore_ascii_case("font-face")
            || at_rule_name.eq_ignore_ascii_case("supports")
            || at_rule_name.eq_ignore_ascii_case("layer")
            || at_rule_name.eq_ignore_ascii_case("position-fallback")
            || at_rule_name.eq_ignore_ascii_case("try")
        {
            self.parse_simple_block().map(Some)?
        } else if at_rule_name.eq_ignore_ascii_case("charset")
            || at_rule_name.eq_ignore_ascii_case("custom-media")
        {
            None
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
}
