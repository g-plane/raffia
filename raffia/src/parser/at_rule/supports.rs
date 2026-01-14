use super::Parser;
use crate::{
    Parse,
    ast::*,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
};

// https://drafts.csswg.org/css-conditional-3/#at-supports
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SupportsCondition<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match &peek!(input).token {
            Token::Ident(token) if token.name().eq_ignore_ascii_case("not") => {
                let keyword = input.parse::<Ident>()?;
                let condition = input.parse::<SupportsInParens>()?;
                let span = Span {
                    start: keyword.span.start,
                    end: condition.span().end,
                };
                Ok(SupportsCondition {
                    conditions: vec![SupportsConditionKind::Not(SupportsNot {
                        keyword,
                        condition,
                        span: span.clone(),
                    })],
                    span,
                })
            }
            _ => {
                let first = input.parse::<SupportsInParens>()?;
                let mut span = first.span().clone();
                let mut conditions = vec![SupportsConditionKind::SupportsInParens(first)];
                while let Token::Ident(ident) = &peek!(input).token {
                    let name = ident.name();
                    if name.eq_ignore_ascii_case("and") {
                        let ident = input.parse::<Ident>()?;
                        let condition = input.parse::<SupportsInParens>()?;
                        let span = Span {
                            start: ident.span.start,
                            end: condition.span().end,
                        };
                        conditions.push(SupportsConditionKind::And(SupportsAnd {
                            keyword: ident,
                            condition,
                            span,
                        }));
                    } else if name.eq_ignore_ascii_case("or") {
                        let ident = input.parse::<Ident>()?;
                        let condition = input.parse::<SupportsInParens>()?;
                        let span = Span {
                            start: ident.span.start,
                            end: condition.span().end,
                        };
                        conditions.push(SupportsConditionKind::Or(SupportsOr {
                            keyword: ident,
                            condition,
                            span,
                        }));
                    } else {
                        break;
                    }
                }
                if let Some(last) = conditions.last() {
                    span.end = last.span().end;
                }
                Ok(SupportsCondition { conditions, span })
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SupportsInParens<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match peek!(input) {
            TokenWithSpan {
                token: Token::LParen(..),
                ..
            } => input
                .try_parse(|parser| {
                    parser.parse::<SupportsDecl>().map(|supports_decl| {
                        let span = supports_decl.span.clone();
                        SupportsInParens {
                            kind: SupportsInParensKind::Feature(Box::new(supports_decl)),
                            span,
                        }
                    })
                })
                .or_else(|_| {
                    let (_, Span { start, .. }) = expect!(input, LParen);
                    let condition = input.parse()?;
                    let (_, Span { end, .. }) = expect!(input, RParen);
                    Ok(SupportsInParens {
                        kind: SupportsInParensKind::SupportsCondition(condition),
                        span: Span { start, end },
                    })
                }),
            TokenWithSpan {
                token: Token::Ident(..),
                ..
            } => {
                let function_ident = input.parse::<Ident>()?;
                if function_ident.name.eq_ignore_ascii_case("selector") {
                    expect!(input, LParen);
                    let selector_list = input.parse::<SelectorList>()?;
                    expect!(input, RParen);
                    let span = selector_list.span.clone();
                    Ok(SupportsInParens {
                        kind: SupportsInParensKind::Selector(selector_list),
                        span,
                    })
                } else {
                    let function =
                        input.parse_function(InterpolableIdent::Literal(function_ident))?;
                    let span = function.span.clone();
                    Ok(SupportsInParens {
                        kind: SupportsInParensKind::Function(function),
                        span,
                    })
                }
            }
            TokenWithSpan { token, span } => Err(Error {
                kind: ErrorKind::Unexpected("'('", token.symbol()),
                span: span.clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SupportsDecl<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LParen).1.start;
        let decl = input.parse()?;
        let end = expect!(input, RParen).1.end;
        Ok(SupportsDecl {
            decl,
            span: Span { start, end },
        })
    }
}
