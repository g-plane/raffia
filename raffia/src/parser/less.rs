use super::Parser;
use crate::{
    ast::*,
    bump,
    config::Syntax,
    eat,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util, Parse,
};

const PRECEDENCE_RELATIONAL: u8 = 3;
const PRECEDENCE_AND: u8 = 2;
const PRECEDENCE_OR: u8 = 1;

const PRECEDENCE_MULTIPLY: u8 = 2;
const PRECEDENCE_PLUS: u8 = 1;

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(super) fn parse_less_condition(
        &mut self,
        needs_parens: bool,
    ) -> PResult<LessCondition<'s>> {
        self.parse_less_condition_recursively(needs_parens, 0)
    }

    fn parse_less_condition_atom(&mut self, needs_parens: bool) -> PResult<LessCondition<'s>> {
        self.try_parse(|parser| parser.parse_less_condition(needs_parens))
            .or_else(|_| self.parse_less_operation().map(LessCondition::Value))
    }

    fn parse_less_condition_recursively(
        &mut self,
        needs_parens: bool,
        precedence: u8,
    ) -> PResult<LessCondition<'s>> {
        let mut left = if precedence >= PRECEDENCE_RELATIONAL {
            match &peek!(self).token {
                Token::LParen(..) => {
                    let Span { start, .. } = bump!(self).span;
                    let condition = self.parse_less_condition_atom(needs_parens)?;
                    let (_, Span { end, .. }) = expect!(self, RParen);
                    LessCondition::Parenthesized(LessParenthesizedCondition {
                        condition: Box::new(condition),
                        span: Span { start, end },
                    })
                }
                Token::Ident(ident) if ident.raw == "not" => {
                    let Span { start, .. } = bump!(self).span;
                    expect!(self, LParen);
                    let condition = self.parse_less_condition_atom(needs_parens)?;
                    let (_, Span { end, .. }) = expect!(self, RParen);
                    LessCondition::Negated(LessNegatedCondition {
                        condition: Box::new(condition),
                        span: Span { start, end },
                    })
                }
                _ => {
                    if needs_parens {
                        use crate::{token::LParen, tokenizer::TokenSymbol};
                        let TokenWithSpan { token, span } = bump!(self);
                        return Err(Error {
                            kind: ErrorKind::Unexpected(LParen::symbol(), token.symbol()),
                            span,
                        });
                    } else {
                        self.parse_less_operation().map(LessCondition::Value)?
                    }
                }
            }
        } else {
            self.parse_less_condition_recursively(needs_parens, precedence + 1)?
        };

        loop {
            let op = match &peek!(self).token {
                Token::GreaterThan(..) if precedence == PRECEDENCE_RELATIONAL => {
                    LessBinaryConditionOperator {
                        kind: LessBinaryConditionOperatorKind::GreaterThan,
                        span: bump!(self).span,
                    }
                }
                Token::GreaterThanEqual(..) if precedence == PRECEDENCE_RELATIONAL => {
                    LessBinaryConditionOperator {
                        kind: LessBinaryConditionOperatorKind::GreaterThanOrEqual,
                        span: bump!(self).span,
                    }
                }
                Token::LessThan(..) if precedence == PRECEDENCE_RELATIONAL => {
                    LessBinaryConditionOperator {
                        kind: LessBinaryConditionOperatorKind::LessThan,
                        span: bump!(self).span,
                    }
                }
                Token::LessThanEqual(..) if precedence == PRECEDENCE_RELATIONAL => {
                    LessBinaryConditionOperator {
                        kind: LessBinaryConditionOperatorKind::LessThanOrEqual,
                        span: bump!(self).span,
                    }
                }
                Token::Equal(..) if precedence == PRECEDENCE_RELATIONAL => {
                    let eq_span = bump!(self).span;
                    match peek!(self) {
                        TokenWithSpan {
                            token: Token::GreaterThan(..),
                            span: gt_span,
                        } if eq_span.end == gt_span.start => LessBinaryConditionOperator {
                            kind: LessBinaryConditionOperatorKind::EqualOrGreaterThan,
                            span: Span {
                                start: eq_span.start,
                                end: bump!(self).span.end,
                            },
                        },
                        TokenWithSpan {
                            token: Token::LessThan(..),
                            span: lt_span,
                        } if eq_span.end == lt_span.start => LessBinaryConditionOperator {
                            kind: LessBinaryConditionOperatorKind::EqualOrLessThan,
                            span: Span {
                                start: eq_span.start,
                                end: bump!(self).span.end,
                            },
                        },
                        _ => LessBinaryConditionOperator {
                            kind: LessBinaryConditionOperatorKind::Equal,
                            span: eq_span,
                        },
                    }
                }
                Token::Ident(token) if token.raw == "and" && precedence == PRECEDENCE_AND => {
                    LessBinaryConditionOperator {
                        kind: LessBinaryConditionOperatorKind::And,
                        span: bump!(self).span,
                    }
                }
                Token::Ident(token) if token.raw == "or" && precedence == PRECEDENCE_OR => {
                    LessBinaryConditionOperator {
                        kind: LessBinaryConditionOperatorKind::Or,
                        span: bump!(self).span,
                    }
                }
                _ => break,
            };

            // multiple conditions in Less is right-associated
            let right = self.parse_less_condition_recursively(needs_parens, precedence)?;

            let span = Span {
                start: left.span().start,
                end: right.span().end,
            };
            left = LessCondition::Binary(LessBinaryCondition {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    pub(super) fn parse_less_interpolated_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let first = match peek!(self) {
            TokenWithSpan {
                token: Token::Ident(..),
                ..
            } => {
                let (ident, ident_span) = expect!(self, Ident);
                match peek!(self) {
                    TokenWithSpan {
                        token: Token::AtLBraceVar(..),
                        span,
                    } if ident_span.end == span.start => {
                        LessInterpolatedIdentElement::Static((ident, ident_span).into())
                    }
                    _ => return Ok(InterpolableIdent::Literal((ident, ident_span).into())),
                }
            }
            TokenWithSpan {
                token: Token::AtLBraceVar(..),
                ..
            } => self.parse().map(LessInterpolatedIdentElement::Variable)?,
            TokenWithSpan { token, span } => {
                use crate::{
                    token::{AtLBraceVar, Ident},
                    tokenizer::TokenSymbol,
                };
                return Err(Error {
                    kind: ErrorKind::ExpectOneOf(
                        vec![Ident::symbol(), AtLBraceVar::symbol()],
                        token.symbol(),
                    ),
                    span: span.clone(),
                });
            }
        };
        let mut span = first.span().clone();

        let mut elements = self.parse_less_interpolated_ident_rest(&mut span.end)?;
        elements.insert(0, first);

        Ok(InterpolableIdent::LessInterpolated(LessInterpolatedIdent {
            elements,
            span,
        }))
    }

    pub(super) fn parse_less_interpolated_ident_rest(
        &mut self,
        end: &mut usize,
    ) -> PResult<Vec<LessInterpolatedIdentElement<'s>>> {
        let mut elements = vec![];
        loop {
            match peek!(self) {
                TokenWithSpan {
                    token: Token::Ident(..),
                    span: ident_span,
                } if *end == ident_span.start => {
                    let (ident, ident_span) = expect!(self, Ident);
                    *end = ident_span.end;
                    elements.push(LessInterpolatedIdentElement::Static(
                        (ident, ident_span).into(),
                    ));
                }
                TokenWithSpan {
                    token: Token::AtLBraceVar(..),
                    span: at_brace_var_span,
                } if *end == at_brace_var_span.start => {
                    let variable = self.parse::<LessVariableInterpolation>()?;
                    *end = variable.span.end;
                    elements.push(LessInterpolatedIdentElement::Variable(variable));
                }
                _ => return Ok(elements),
            }
        }
    }

    fn parse_less_operation(&mut self) -> PResult<ComponentValue<'s>> {
        self.parse_less_operation_recursively(0)
    }

    fn parse_less_operation_recursively(&mut self, precedence: u8) -> PResult<ComponentValue<'s>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            if eat!(self, LParen).is_some() {
                let operation = self.parse_less_operation()?;
                expect!(self, RParen);
                operation
            } else {
                self.parse_component_value_atom()?
            }
        } else {
            self.parse_less_operation_recursively(precedence + 1)?
        };

        loop {
            let op = match peek!(self).token {
                Token::Asterisk(..) if precedence == PRECEDENCE_MULTIPLY => LessOperationOperator {
                    kind: LessOperationOperatorKind::Multiply,
                    span: bump!(self).span,
                },
                Token::Solidus(..) if precedence == PRECEDENCE_MULTIPLY => LessOperationOperator {
                    kind: LessOperationOperatorKind::Division,
                    span: bump!(self).span,
                },
                Token::Dot(..) if precedence == PRECEDENCE_MULTIPLY => {
                    // `./` is also division
                    let Span { start, .. } = bump!(self).span;
                    let (_, Span { end, .. }) = expect_without_ws_or_comments!(self, Solidus);
                    LessOperationOperator {
                        kind: LessOperationOperatorKind::Division,
                        span: Span { start, end },
                    }
                }
                Token::Plus(..) if precedence == PRECEDENCE_PLUS => LessOperationOperator {
                    kind: LessOperationOperatorKind::Plus,
                    span: bump!(self).span,
                },
                Token::Minus(..) if precedence == PRECEDENCE_PLUS => LessOperationOperator {
                    kind: LessOperationOperatorKind::Minus,
                    span: bump!(self).span,
                },
                _ => break,
            };

            let right = self.parse_less_operation_recursively(precedence + 1)?;
            let span = Span {
                start: left.span().start,
                end: right.span().end,
            };
            left = ComponentValue::LessOperation(LessOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessConditions<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse_less_condition(true)?;
        let mut span = first.span().clone();

        let mut conditions = vec![first];
        while eat!(input, Comma).is_some() {
            conditions.push(input.parse_less_condition(true)?);
        }

        if let Some(last) = conditions.last() {
            span.end = last.span().end;
        }
        Ok(LessConditions { conditions, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessDetachedRuleset<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let block = input.parse::<SimpleBlock>()?;
        let span = block.span.clone();
        Ok(LessDetachedRuleset { block, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessEscapedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, Tilde);
        let str = input.parse::<Str>()?;
        let span = Span {
            start,
            end: str.span.end,
        };
        Ok(LessEscapedStr { str, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessInterpolatedStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (first, first_span) = expect!(input, StrTemplate);
        let quote = first.raw.chars().next().unwrap();
        debug_assert!(quote == '\'' || quote == '"');
        let mut span = first_span.clone();
        let mut elements = vec![LessInterpolatedStrElement::Static(
            (first, first_span).into(),
        )];

        let mut is_parsing_static_part = false;
        loop {
            if is_parsing_static_part {
                let (token, str_tpl_span) = input.tokenizer.scan_string_template(quote)?;
                let tail = token.tail;
                let end = str_tpl_span.end;
                elements.push(LessInterpolatedStrElement::Static(
                    (token, str_tpl_span).into(),
                ));
                if tail {
                    span.end = end;
                    break;
                }
            } else {
                // '@' is consumed, so '{' left only
                let start = expect!(input, LBrace).1.start - 1;
                let (name, name_span) = expect_without_ws_or_comments!(input, Ident);

                let end = expect!(input, RBrace).1.end;
                elements.push(LessInterpolatedStrElement::Variable(
                    LessVariableInterpolation {
                        name: (name, name_span).into(),
                        span: Span { start, end },
                    },
                ));
            }
            is_parsing_static_part = !is_parsing_static_part;
        }

        Ok(LessInterpolatedStr { elements, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessJavaScriptSnippet<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let tilde = eat!(input, Tilde);
        let (token, span) = expect!(input, BacktickCode);

        Ok(LessJavaScriptSnippet {
            code: &token.raw[1..token.raw.len() - 1],
            raw: token.raw,
            escaped: tilde.is_some(),
            span: Span {
                start: tilde.map(|(_, span)| span.start).unwrap_or(span.start),
                end: span.end,
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessMixinDefinition<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let (name, start) = match bump!(input) {
            TokenWithSpan {
                token: Token::Dot(..),
                span,
            } => {
                let (ident, _) = expect_without_ws_or_comments!(input, Ident);
                (
                    if ident.escaped {
                        format!(".{}", util::handle_escape(ident.raw))
                    } else {
                        format!(".{}", ident.raw)
                    },
                    span.start,
                )
            }
            TokenWithSpan {
                token: Token::Hash(hash),
                span,
            } => (
                if hash.escaped {
                    format!("#{}", util::handle_escape(hash.raw))
                } else {
                    format!("#{}", hash.raw)
                },
                span.start,
            ),
            TokenWithSpan { token, span } => {
                use crate::{
                    token::{Dot, Hash},
                    tokenizer::TokenSymbol,
                };
                return Err(Error {
                    kind: ErrorKind::ExpectOneOf(
                        vec![Dot::symbol(), Hash::symbol()],
                        token.symbol(),
                    ),
                    span,
                });
            }
        };

        expect!(input, LParen);
        let mut semicolon_comes_at = 0;
        let mut params = vec![];
        while eat!(input, RParen).is_none() {
            if let Some((_, span)) = eat!(input, DotDotDot) {
                params.push(LessParameter::Variadic(LessVariadicParameter {
                    name: None,
                    span,
                }));
                eat!(input, Semicolon);
                expect!(input, RParen);
                break;
            } else {
                match input.parse()? {
                    value @ ComponentValue::LessVariable(..)
                    | value @ ComponentValue::LessPropertyVariable(..) => {
                        let (name, name_span) = match value {
                            ComponentValue::LessVariable(LessVariable { name, span }) => {
                                (format!("@{}", name.name), span)
                            }
                            ComponentValue::LessPropertyVariable(LessPropertyVariable {
                                name,
                                span,
                            }) => (format!("${}", name.name), span),
                            _ => unreachable!(),
                        };
                        if eat!(input, Colon).is_some() {
                            let value = if matches!(peek!(input).token, Token::LBrace(..)) {
                                input.parse().map(ComponentValue::LessDetachedRuleset)?
                            } else {
                                input.parse::<ComponentValue>()?
                            };
                            let span = Span {
                                start,
                                end: value.span().end,
                            };
                            params.push(LessParameter::Named(LessNamedParameter {
                                name,
                                value: Some(value),
                                span,
                            }));
                        } else if let Some((_, Span { end, .. })) = eat!(input, DotDotDot) {
                            params.push(LessParameter::Variadic(LessVariadicParameter {
                                name: Some(name),
                                span: Span { start, end },
                            }));
                            eat!(input, Semicolon);
                            expect!(input, RParen);
                            break;
                        } else {
                            params.push(LessParameter::Named(LessNamedParameter {
                                name,
                                value: None,
                                span: name_span,
                            }));
                        }
                    }
                    value => {
                        let span = value.span().clone();
                        params.push(LessParameter::Unnamed(LessUnnamedParameter { value, span }));
                    }
                }

                match bump!(input) {
                    TokenWithSpan {
                        token: Token::RParen(..),
                        ..
                    } => {
                        if semicolon_comes_at > 0 {
                            wrap_less_params_into_less_list(&mut params, semicolon_comes_at)
                                .map_err(|kind| Error {
                                    kind,
                                    span: Span {
                                        // We've checked `semicolon_comes_at` must be greater than 0,
                                        // so `params` won't be empty.
                                        start: params.first().unwrap().span().start,
                                        end: params.last().unwrap().span().end,
                                    },
                                })?;
                        }
                        break;
                    }
                    TokenWithSpan {
                        token: Token::Comma(..),
                        ..
                    } => {}
                    TokenWithSpan {
                        token: Token::Semicolon(..),
                        span,
                    } => {
                        wrap_less_params_into_less_list(&mut params, semicolon_comes_at)
                            .map_err(|kind| Error { kind, span })?;
                        semicolon_comes_at = params.len();
                    }
                    TokenWithSpan { token, span } => {
                        use crate::{token::RParen, tokenizer::TokenSymbol};
                        return Err(Error {
                            kind: ErrorKind::Unexpected(RParen::symbol(), token.symbol()),
                            span,
                        });
                    }
                }
            }
        }

        let guard = match &peek!(input).token {
            Token::Ident(ident) if ident.raw == "when" => {
                bump!(input);
                Some(input.parse()?)
            }
            _ => None,
        };

        let block = input.parse::<SimpleBlock>()?;

        let span = Span {
            start,
            end: block.span.end,
        };

        Ok(LessMixinDefinition {
            name,
            params,
            guard,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Option<LessPropertyMerge> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        match &peek!(input).token {
            Token::Plus(..) => Ok(Some(LessPropertyMerge {
                kind: LessPropertyMergeKind::Comma,
                span: bump!(input).span,
            })),
            Token::PlusUnderscore(..) => Ok(Some(LessPropertyMerge {
                kind: LessPropertyMergeKind::Space,
                span: bump!(input).span,
            })),
            _ => Ok(None),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessPropertyVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (dollar_var, span) = expect!(input, DollarVar);
        Ok(LessPropertyVariable {
            name: Ident::from((
                dollar_var.ident,
                Span {
                    start: span.start + 1,
                    end: span.end,
                },
            )),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (at_keyword, span) = expect!(input, AtKeyword);
        Ok(LessVariable {
            name: (
                at_keyword.ident,
                Span {
                    start: span.start + 1,
                    end: span.end,
                },
            )
                .into(),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let name = input.parse::<LessVariable>()?;
        expect!(input, Colon);
        let value = if matches!(peek!(input).token, Token::LBrace(..)) {
            let detached_ruleset = input.parse::<LessDetachedRuleset>()?;
            let span = detached_ruleset.span.clone();
            ComponentValues {
                values: vec![ComponentValue::LessDetachedRuleset(detached_ruleset)],
                span,
            }
        } else {
            input.parse_component_values(/* allow_comma */ true)?
        };

        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(LessVariableDeclaration { name, value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableInterpolation<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (at_lbrace_var, span) = expect!(input, AtLBraceVar);
        Ok(LessVariableInterpolation {
            name: (
                at_lbrace_var.ident,
                Span {
                    start: span.start + 2,
                    end: span.end - 1,
                },
            )
                .into(),
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableVariable<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, at_span) = expect!(input, At);
        let variable = input.parse::<LessVariable>()?;
        util::assert_no_ws_or_comment(&at_span, &variable.span)?;

        let span = Span {
            start: at_span.start,
            end: variable.span.end,
        };
        Ok(LessVariableVariable { variable, span })
    }
}

fn wrap_less_params_into_less_list(
    params: &mut Vec<LessParameter<'_>>,
    index: usize,
) -> Result<(), ErrorKind> {
    if let [first, .., last] = &params[index..] {
        let span = Span {
            start: first.span().start,
            end: last.span().end,
        };
        let elements = params
            .drain(index..)
            .map(|param| {
                if let LessParameter::Unnamed(LessUnnamedParameter { value, .. }) = param {
                    Ok(value)
                } else {
                    // reject code like this:
                    // .mixin(@a: 5, @b: 6; @c: 7) {}
                    // .mixin(@a: 5; @b: 6, @c: 7) {}
                    Err(ErrorKind::MixedDelimiterKindInLessMixin)
                }
            })
            .collect::<Result<_, _>>()?;
        params.push(LessParameter::Unnamed(LessUnnamedParameter {
            value: ComponentValue::LessList(LessList {
                elements,
                span: span.clone(),
            }),
            span,
        }));
    }
    Ok(())
}
