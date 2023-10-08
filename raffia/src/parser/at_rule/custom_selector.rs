use super::Parser;
use crate::{ast::*, error::PResult, expect, peek, pos::Span, tokenizer::Token, util, Parse};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CustomSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let prefix_arg = if matches!(peek!(input).token, Token::DollarVar(..)) {
            Some(input.parse::<CustomSelectorArg>()?)
        } else {
            None
        };

        let (_, colon_span) = expect!(input, Colon);
        if let Some(prefix_arg) = &prefix_arg {
            util::assert_no_ws_or_comment(&prefix_arg.span, &colon_span)?;
        }
        let name = input.parse::<Ident>()?;
        util::assert_no_ws_or_comment(&colon_span, &name.span)?;

        let args = if matches!(peek!(input).token, Token::LParen(..)) {
            Some(input.parse::<CustomSelectorArgs>()?)
        } else {
            None
        };

        let span = Span {
            start: prefix_arg
                .as_ref()
                .map(|prefix_arg| prefix_arg.span.start)
                .unwrap_or(name.span.start),
            end: args
                .as_ref()
                .map(|args| args.span.end)
                .unwrap_or(name.span.end),
        };
        Ok(CustomSelector {
            prefix_arg,
            name,
            args,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CustomSelectorArg<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (dollar_var, dollar_var_span) = expect!(input, DollarVar);
        Ok(CustomSelectorArg {
            name: (
                dollar_var.ident,
                Span {
                    start: dollar_var_span.start + 1,
                    end: dollar_var_span.end,
                },
            )
                .into(),
            span: dollar_var_span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CustomSelectorArgs<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, LParen);

        let mut args = vec![];
        let mut comma_spans = vec![];
        while !matches!(peek!(input).token, Token::RParen(..)) {
            args.push(input.parse()?);
            if !matches!(peek!(input).token, Token::RParen(..)) {
                comma_spans.push(expect!(input, Comma).1);
            }
        }

        let (_, Span { end, .. }) = expect!(input, RParen);
        Ok(CustomSelectorArgs {
            args,
            comma_spans,
            span: Span { start, end },
        })
    }
}

// https://drafts.csswg.org/css-extensions/#custom-selectors
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CustomSelectorPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let custom_selector = input.parse::<CustomSelector>()?;
        let selector = input.parse::<SelectorList>()?;
        let span = Span {
            start: custom_selector.span.start,
            end: selector.span.end,
        };
        Ok(CustomSelectorPrelude {
            custom_selector,
            selector,
            span,
        })
    }
}
