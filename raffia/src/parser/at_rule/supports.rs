use super::Parser;
use crate::{
    ast::*,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_supports_condition(&mut self) -> PResult<SupportsCondition<'s>> {
        match self.tokenizer.peek()? {
            Token::Ident(token) if token.name.eq_ignore_ascii_case("not") => {
                let ident = self.parse_ident()?;
                let condition = self.parse_supports_in_parens()?;
                let span = Span {
                    start: ident.span.start,
                    end: condition.span().end,
                };
                Ok(SupportsCondition {
                    conditions: vec![SupportsConditionKind::Not(SupportsNot {
                        ident,
                        condition,
                        span: span.clone(),
                    })],
                    span,
                })
            }
            _ => {
                let first = self.parse_supports_in_parens()?;
                let mut span = first.span().clone();
                let mut conditions = vec![SupportsConditionKind::SupportsInParens(first)];
                loop {
                    match self.tokenizer.peek()? {
                        Token::Ident(token) if token.name.eq_ignore_ascii_case("and") => {
                            let ident = self.parse_ident()?;
                            let condition = self.parse_supports_in_parens()?;
                            let span = Span {
                                start: ident.span.start,
                                end: condition.span().end,
                            };
                            conditions.push(SupportsConditionKind::And(SupportsAnd {
                                ident,
                                condition,
                                span,
                            }));
                        }
                        Token::Ident(token) if token.name.eq_ignore_ascii_case("or") => {
                            let ident = self.parse_ident()?;
                            let condition = self.parse_supports_in_parens()?;
                            let span = Span {
                                start: ident.span.start,
                                end: condition.span().end,
                            };
                            conditions.push(SupportsConditionKind::Or(SupportsOr {
                                ident,
                                condition,
                                span,
                            }));
                        }
                        _ => break,
                    }
                }
                if let Some(last) = conditions.last() {
                    span.end = last.span().end;
                }
                Ok(SupportsCondition { conditions, span })
            }
        }
    }

    fn parse_supports_in_parens(&mut self) -> PResult<SupportsInParens<'s>> {
        match self.tokenizer.peek()? {
            Token::LParen(..) => {
                if let Some(supports_decl) = self.try_parse(|parser| parser.parse_supports_decl()) {
                    Ok(SupportsInParens::Feature(supports_decl))
                } else {
                    expect!(self, LParen);
                    let condition = self.parse_supports_condition()?;
                    expect!(self, RParen);
                    Ok(SupportsInParens::SupportsCondition(condition))
                }
            }
            token => Err(Error {
                kind: ErrorKind::Unexpected("LParen"),
                span: token.span().clone(),
            }),
        }
    }

    fn parse_supports_decl(&mut self) -> PResult<SupportsDecl<'s>> {
        let l_paren = expect!(self, LParen);
        let decl = self.parse_declaration()?;
        let r_paren = expect!(self, RParen);
        Ok(SupportsDecl {
            decl,
            span: Span {
                start: l_paren.span.start,
                end: r_paren.span.end,
            },
        })
    }
}
