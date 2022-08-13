use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
    Parse,
};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ContainerCondition<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("not") => {
                let container_condition_not = input.parse::<ContainerConditionNot>()?;
                let span = container_condition_not.span.clone();
                Ok(ContainerCondition {
                    conditions: vec![ContainerConditionKind::Not(container_condition_not)],
                    span,
                })
            }
            _ => {
                let first = input.parse::<QueryInParens>()?;
                let mut span = first.span().clone();
                let mut conditions = vec![ContainerConditionKind::QueryInParens(first)];
                match input.tokenizer.peek()? {
                    Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => loop {
                        conditions.push(ContainerConditionKind::And(input.parse()?));
                        match input.tokenizer.peek()? {
                            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => {}
                            _ => break,
                        }
                    },
                    Token::Ident(ident) if ident.name.eq_ignore_ascii_case("or") => loop {
                        conditions.push(ContainerConditionKind::Or(input.parse()?));
                        match input.tokenizer.peek()? {
                            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("or") => {}
                            _ => break,
                        }
                    },
                    _ => {}
                }

                if let Some(last) = conditions.last() {
                    span.end = last.span().end;
                }
                Ok(ContainerCondition { conditions, span })
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ContainerConditionAnd<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("and") {
            let query_in_parens = input.parse::<QueryInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: query_in_parens.span().end,
            };
            Ok(ContainerConditionAnd {
                ident,
                query_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectContainerConditionAnd,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ContainerConditionNot<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("not") {
            let query_in_parens = input.parse::<QueryInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: query_in_parens.span().end,
            };
            Ok(ContainerConditionNot {
                ident,
                query_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectContainerConditionNot,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ContainerConditionOr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("or") {
            let query_in_parens = input.parse::<QueryInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: query_in_parens.span().end,
            };
            Ok(ContainerConditionOr {
                ident,
                query_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectContainerConditionOr,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for QueryInParens<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if eat!(input, LParen).is_some() {
            let query_in_parens =
                if let Some(container_condition) = input.try_parse(|parser| parser.parse()) {
                    QueryInParens::ContainerCondition(container_condition)
                } else {
                    input
                        .parse_media_feature()
                        .map(QueryInParens::SizeFeature)?
                };
            expect!(input, RParen);
            Ok(query_in_parens)
        } else {
            let style_keyword = expect!(input, Ident);
            if !style_keyword.name.eq_ignore_ascii_case("style") {
                return Err(Error {
                    kind: ErrorKind::ExpectStyleQuery,
                    span: style_keyword.span,
                });
            }
            let l_paren = expect!(input, LParen);
            input.assert_no_ws_or_comment(&style_keyword.span, &l_paren.span)?;
            let query_in_parens = input.parse().map(QueryInParens::StyleQuery)?;
            expect!(input, RParen);
            Ok(query_in_parens)
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for StyleCondition<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("not") => {
                let style_condition_not = input.parse::<StyleConditionNot>()?;
                let span = style_condition_not.span.clone();
                Ok(StyleCondition {
                    conditions: vec![StyleConditionKind::Not(style_condition_not)],
                    span,
                })
            }
            _ => {
                let first = input.parse::<StyleInParens>()?;
                let mut span = first.span().clone();
                let mut conditions = vec![StyleConditionKind::StyleInParens(first)];
                match input.tokenizer.peek()? {
                    Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => loop {
                        conditions.push(StyleConditionKind::And(input.parse()?));
                        match input.tokenizer.peek()? {
                            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => {}
                            _ => break,
                        }
                    },
                    Token::Ident(ident) if ident.name.eq_ignore_ascii_case("or") => loop {
                        conditions.push(StyleConditionKind::Or(input.parse()?));
                        match input.tokenizer.peek()? {
                            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("or") => {}
                            _ => break,
                        }
                    },
                    _ => {}
                }

                if let Some(last) = conditions.last() {
                    span.end = last.span().end;
                }
                Ok(StyleCondition { conditions, span })
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for StyleConditionAnd<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("and") {
            let style_in_parens = input.parse::<StyleInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: style_in_parens.span().end,
            };
            Ok(StyleConditionAnd {
                ident,
                style_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectStyleConditionAnd,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for StyleConditionNot<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("not") {
            let style_in_parens = input.parse::<StyleInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: style_in_parens.span().end,
            };
            Ok(StyleConditionNot {
                ident,
                style_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectStyleConditionNot,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for StyleConditionOr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("or") {
            let style_in_parens = input.parse::<StyleInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: style_in_parens.span().end,
            };
            Ok(StyleConditionOr {
                ident,
                style_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectStyleConditionOr,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for StyleInParens<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        expect!(input, LParen);
        let style_in_parens =
            if let Some(style_condition) = input.try_parse(|parser| parser.parse()) {
                StyleInParens::Condition(style_condition)
            } else {
                StyleInParens::Feature(input.parse()?)
            };
        expect!(input, RParen);
        Ok(style_in_parens)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for StyleQuery<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if let Some(condition) = input.try_parse(|parser| parser.parse()) {
            Ok(StyleQuery::Condition(condition))
        } else {
            input.parse().map(StyleQuery::Feature)
        }
    }
}

// https://drafts.csswg.org/css-contain-3/#container-rule
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ContainerPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let name = input.try_parse(|parser| match parser.parse_interpolable_ident()? {
            InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("not") => {
                Err(Error {
                    kind: ErrorKind::TryParseError,
                    span: ident.span,
                })
            }
            InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("style") => {
                match parser.tokenizer.peek()? {
                    Token::LParen(l_paren) if l_paren.span.start == ident.span.end => Err(Error {
                        kind: ErrorKind::TryParseError,
                        span: ident.span,
                    }),
                    _ => Ok(InterpolableIdent::Literal(ident)),
                }
            }
            ident => Ok(ident),
        });
        let condition = input.parse::<ContainerCondition>()?;
        let mut span = condition.span().clone();
        if let Some(name) = &name {
            span.start = name.span().start;
        }
        Ok(ContainerPrelude {
            name,
            condition,
            span,
        })
    }
}
