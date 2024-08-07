use super::{
    state::{ParserState, QualifiedRuleContext, LESS_CTX_ALLOW_DIV, LESS_CTX_ALLOW_KEYFRAME_BLOCK},
    Parser,
};
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
use std::{borrow::Cow, mem};

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

    fn parse_less_condition_atom(&mut self) -> PResult<LessCondition<'s>> {
        let left = self
            .parse_less_operation(/* allow_mixin_call */ false)
            .map(LessCondition::Value)?;

        let op = match &peek!(self).token {
            Token::GreaterThan(..) => LessBinaryConditionOperator {
                kind: LessBinaryConditionOperatorKind::GreaterThan,
                span: bump!(self).span,
            },
            Token::GreaterThanEqual(..) => LessBinaryConditionOperator {
                kind: LessBinaryConditionOperatorKind::GreaterThanOrEqual,
                span: bump!(self).span,
            },
            Token::LessThan(..) => LessBinaryConditionOperator {
                kind: LessBinaryConditionOperatorKind::LessThan,
                span: bump!(self).span,
            },
            Token::LessThanEqual(..) => LessBinaryConditionOperator {
                kind: LessBinaryConditionOperatorKind::LessThanOrEqual,
                span: bump!(self).span,
            },
            Token::Equal(..) => {
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
            _ => return Ok(left),
        };

        let right = self
            .parse_less_operation(/* allow_mixin_call */ false)
            .map(LessCondition::Value)?;

        let span = Span {
            start: left.span().start,
            end: right.span().end,
        };
        Ok(LessCondition::Binary(LessBinaryCondition {
            left: Box::new(left),
            op,
            right: Box::new(right),
            span,
        }))
    }

    fn parse_less_condition_inside_parens(
        &mut self,
        needs_parens: bool,
    ) -> PResult<LessCondition<'s>> {
        self.try_parse(|parser| {
            let condition = parser.parse_less_condition(needs_parens);
            match &condition {
                Ok(LessCondition::Parenthesized(LessParenthesizedCondition {
                    condition: inner_condition,
                    span,
                })) => match &**inner_condition {
                    LessCondition::Value(ComponentValue::LessBinaryOperation(..))
                        if matches!(
                            peek!(parser).token,
                            Token::GreaterThan(..)
                                | Token::GreaterThanEqual(..)
                                | Token::LessThan(..)
                                | Token::LessThanEqual(..)
                                | Token::Equal(..)
                                | Token::Plus(..)
                                | Token::Minus(..)
                                | Token::Asterisk(..)
                                | Token::Solidus(..)
                        ) =>
                    {
                        // special case:
                        // `when ((8 + 6) > 13)`
                        // the `(8 + 6)` above is operation, not condition
                        Err(Error {
                            kind: ErrorKind::TryParseError,
                            span: span.clone(),
                        })
                    }
                    _ => condition,
                },
                _ => condition,
            }
        })
        .or_else(|_| self.parse_less_condition_atom())
    }

    fn parse_less_condition_recursively(
        &mut self,
        needs_parens: bool,
        precedence: u8,
    ) -> PResult<LessCondition<'s>> {
        let mut left = if precedence >= PRECEDENCE_AND {
            match &peek!(self).token {
                Token::LParen(..) => {
                    let Span { start, .. } = bump!(self).span;
                    let condition = self.parse_less_condition_inside_parens(needs_parens)?;
                    let (_, Span { end, .. }) = expect!(self, RParen);
                    LessCondition::Parenthesized(LessParenthesizedCondition {
                        condition: Box::new(condition),
                        span: Span { start, end },
                    })
                }
                Token::Ident(ident) if ident.raw == "not" => {
                    let Span { start, .. } = bump!(self).span;
                    expect!(self, LParen);
                    let condition = self.parse_less_condition_inside_parens(needs_parens)?;
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
                        self.parse_less_condition_atom()?
                    }
                }
            }
        } else {
            self.parse_less_condition_recursively(needs_parens, precedence + 1)?
        };

        loop {
            let op = match &peek!(self).token {
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

            // multiple conditions in Less are right-associated
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

        let (first, Span { start, mut end }) = match peek!(self) {
            TokenWithSpan {
                token: Token::Ident(..),
                ..
            } => {
                let (ident, ident_span) = expect!(self, Ident);
                (
                    LessInterpolatedIdentElement::Static((ident, ident_span.clone()).into()),
                    ident_span,
                )
            }
            TokenWithSpan {
                token: Token::AtLBraceVar(..),
                ..
            } => {
                let interpolation = self.parse::<LessVariableInterpolation>()?;
                let span = interpolation.span.clone();
                (LessInterpolatedIdentElement::Variable(interpolation), span)
            }
            TokenWithSpan {
                token: Token::DollarLBraceVar(..),
                ..
            } if matches!(
                self.state.qualified_rule_ctx,
                Some(QualifiedRuleContext::DeclarationName)
            ) =>
            {
                let interpolation = self.parse::<LessPropertyInterpolation>()?;
                let span = interpolation.span.clone();
                (LessInterpolatedIdentElement::Property(interpolation), span)
            }
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

        let mut elements = self.parse_less_interpolated_ident_rest(&mut end)?;
        if elements.is_empty() {
            if let LessInterpolatedIdentElement::Static(ident) = first {
                return Ok(InterpolableIdent::Literal(Ident {
                    name: ident.value,
                    raw: ident.raw,
                    span: ident.span,
                }));
            }
        }

        elements.insert(0, first);
        Ok(InterpolableIdent::LessInterpolated(LessInterpolatedIdent {
            elements,
            span: Span { start, end },
        }))
    }

    pub(super) fn parse_less_interpolated_ident_rest(
        &mut self,
        end: &mut usize,
    ) -> PResult<Vec<LessInterpolatedIdentElement<'s>>> {
        let mut elements = vec![];
        loop {
            if let Some((token, span)) = self.tokenizer.scan_ident_template()? {
                *end = span.end;
                elements.push(LessInterpolatedIdentElement::Static((token, span).into()));
            } else {
                match peek!(self) {
                    TokenWithSpan {
                        token: Token::AtLBraceVar(..),
                        span: at_lbrace_var_span,
                    } if *end == at_lbrace_var_span.start => {
                        let variable = self.parse::<LessVariableInterpolation>()?;
                        *end = variable.span.end;
                        elements.push(LessInterpolatedIdentElement::Variable(variable));
                    }
                    TokenWithSpan {
                        token: Token::DollarLBraceVar(..),
                        span: dollar_lbrace_var_span,
                    } if matches!(
                        self.state.qualified_rule_ctx,
                        Some(QualifiedRuleContext::DeclarationName)
                    ) && *end == dollar_lbrace_var_span.start =>
                    {
                        let property = self.parse::<LessPropertyInterpolation>()?;
                        *end = property.span.end;
                        elements.push(LessInterpolatedIdentElement::Property(property));
                    }
                    _ => return Ok(elements),
                }
            }
        }
    }

    pub(super) fn parse_less_maybe_mixin_call_or_with_lookups(
        &mut self,
    ) -> PResult<ComponentValue<'s>> {
        let mixin_call = self.parse::<LessMixinCall>()?;
        if matches!(peek!(self).token, Token::LBracket(..)) {
            let lookups = self.parse::<LessLookups>()?;
            let span = Span {
                start: mixin_call.span.start,
                end: lookups.span.end,
            };
            Ok(ComponentValue::LessNamespaceValue(Box::new(
                LessNamespaceValue {
                    callee: LessNamespaceValueCallee::LessMixinCall(mixin_call),
                    lookups,
                    span,
                },
            )))
        } else {
            Ok(ComponentValue::LessMixinCall(mixin_call))
        }
    }

    pub(super) fn parse_less_maybe_variable_or_with_lookups(
        &mut self,
    ) -> PResult<ComponentValue<'s>> {
        let variable = self.parse::<LessVariable>()?;
        match peek!(self) {
            TokenWithSpan {
                token: Token::LBracket(..),
                span,
            } if variable.span.end == span.start => {
                let lookups = self.parse::<LessLookups>()?;
                let span = Span {
                    start: variable.span.start,
                    end: lookups.span.end,
                };
                Ok(ComponentValue::LessNamespaceValue(Box::new(
                    LessNamespaceValue {
                        callee: LessNamespaceValueCallee::LessVariable(variable),
                        lookups,
                        span,
                    },
                )))
            }
            _ => Ok(ComponentValue::LessVariable(variable)),
        }
    }

    pub(super) fn parse_less_operation(
        &mut self,
        allow_mixin_call: bool,
    ) -> PResult<ComponentValue<'s>> {
        self.parse_less_operation_recursively(allow_mixin_call, 0)
    }

    fn parse_less_operation_recursively(
        &mut self,
        allow_mixin_call: bool,
        precedence: u8,
    ) -> PResult<ComponentValue<'s>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            match peek!(self).token {
                Token::LParen(..) => self
                    .parse_less_parenthesized_operation(allow_mixin_call)
                    .map(ComponentValue::LessParenthesizedOperation)?,
                Token::Minus(..) => self
                    .parse::<LessNegativeValue>()
                    .map(ComponentValue::LessNegativeValue)?,
                _ => {
                    let value = self.parse_component_value_atom()?;
                    if let ComponentValue::LessMixinCall(mixin_call) = &value {
                        if !allow_mixin_call {
                            self.recoverable_errors.push(Error {
                                kind: ErrorKind::UnexpectedLessMixinCall,
                                span: mixin_call.span.clone(),
                            });
                        }
                    }
                    value
                }
            }
        } else {
            self.parse_less_operation_recursively(allow_mixin_call, precedence + 1)?
        };

        loop {
            let op = match peek!(self) {
                TokenWithSpan {
                    token: Token::Asterisk(..),
                    ..
                } if precedence == PRECEDENCE_MULTIPLY => LessOperationOperator {
                    kind: LessOperationOperatorKind::Multiply,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Solidus(..),
                    ..
                } if precedence == PRECEDENCE_MULTIPLY
                    && (self.state.less_ctx & LESS_CTX_ALLOW_DIV != 0
                        || can_be_division_operand(&left)) =>
                {
                    LessOperationOperator {
                        kind: LessOperationOperatorKind::Division,
                        span: bump!(self).span,
                    }
                }
                TokenWithSpan {
                    token: Token::Dot(..),
                    ..
                } if precedence == PRECEDENCE_MULTIPLY => {
                    // `./` is also division
                    let Span { start, .. } = bump!(self).span;
                    let (_, Span { end, .. }) = expect_without_ws_or_comments!(self, Solidus);
                    LessOperationOperator {
                        kind: LessOperationOperatorKind::Division,
                        span: Span { start, end },
                    }
                }
                TokenWithSpan {
                    token: Token::Plus(..),
                    ..
                } if precedence == PRECEDENCE_PLUS => LessOperationOperator {
                    kind: LessOperationOperatorKind::Plus,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Minus(..),
                    ..
                } if precedence == PRECEDENCE_PLUS => LessOperationOperator {
                    kind: LessOperationOperatorKind::Minus,
                    span: bump!(self).span,
                },
                TokenWithSpan {
                    token: Token::Number(token),
                    span,
                } if precedence == PRECEDENCE_PLUS
                    && (token.raw.starts_with('+')
                        || token.raw.starts_with('-') && span.start == left.span().end) =>
                {
                    let (number, number_span) = expect!(self, Number);
                    let op = LessOperationOperator {
                        kind: if number.raw.starts_with('+') {
                            LessOperationOperatorKind::Plus
                        } else {
                            LessOperationOperatorKind::Minus
                        },
                        span: Span {
                            start: number_span.start,
                            end: number_span.start + 1,
                        },
                    };
                    let span = Span {
                        start: left.span().start,
                        end: number_span.end,
                    };
                    let right = {
                        let span = Span {
                            start: number_span.start + 1,
                            end: number_span.end,
                        };
                        let raw = unsafe { number.raw.get_unchecked(1..number.raw.len()) };
                        raw.parse()
                            .map_err(|_| Error {
                                kind: ErrorKind::InvalidNumber,
                                span: span.clone(),
                            })
                            .map(|value| ComponentValue::Number(Number { value, raw, span }))?
                    };
                    left = ComponentValue::LessBinaryOperation(LessBinaryOperation {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        span,
                    });
                    continue;
                }
                TokenWithSpan {
                    token: Token::Dimension(token),
                    span,
                } if precedence == PRECEDENCE_PLUS
                    && (token.value.raw.starts_with('+')
                        || token.value.raw.starts_with('-') && span.start == left.span().end) =>
                {
                    let (dimension, dimension_span) = expect!(self, Dimension);
                    let op = LessOperationOperator {
                        kind: if dimension.value.raw.starts_with('+') {
                            LessOperationOperatorKind::Plus
                        } else {
                            LessOperationOperatorKind::Minus
                        },
                        span: Span {
                            start: dimension_span.start,
                            end: dimension_span.start + 1,
                        },
                    };
                    let span = Span {
                        start: left.span().start,
                        end: dimension_span.end,
                    };
                    let right = {
                        (
                            crate::token::Dimension {
                                value: crate::token::Number {
                                    raw: unsafe {
                                        dimension
                                            .value
                                            .raw
                                            .get_unchecked(1..dimension.value.raw.len())
                                    },
                                },
                                unit: dimension.unit,
                            },
                            Span {
                                start: dimension_span.start + 1,
                                end: dimension_span.end,
                            },
                        )
                            .try_into()
                            .map(ComponentValue::Dimension)?
                    };
                    left = ComponentValue::LessBinaryOperation(LessBinaryOperation {
                        left: Box::new(left),
                        op,
                        right: Box::new(right),
                        span,
                    });
                    continue;
                }
                _ => break,
            };

            let right = self.parse_less_operation_recursively(allow_mixin_call, precedence + 1)?;
            let span = Span {
                start: left.span().start,
                end: right.span().end,
            };
            left = ComponentValue::LessBinaryOperation(LessBinaryOperation {
                left: Box::new(left),
                op,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    fn parse_less_parenthesized_operation(
        &mut self,
        allow_mixin_call: bool,
    ) -> PResult<LessParenthesizedOperation<'s>> {
        let (_, Span { start, .. }) = expect!(self, LParen);
        let operation = self
            .with_state(ParserState {
                less_ctx: self.state.less_ctx | LESS_CTX_ALLOW_DIV,
                ..self.state.clone()
            })
            .parse_less_operation(allow_mixin_call)?;
        let (_, Span { end, .. }) = expect!(self, RParen);
        Ok(LessParenthesizedOperation {
            operation: Box::new(operation),
            span: Span { start, end },
        })
    }

    pub(super) fn parse_less_qualified_rule(&mut self) -> PResult<Statement<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let selector_list = self
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::Selector),
                ..self.state
            })
            .parse::<SelectorList>()?;

        match &peek!(self).token {
            Token::Ident(ident) if ident.raw == "when" => {
                let guard = self.parse::<LessConditions>()?;
                let block = self.parse::<SimpleBlock>()?;
                let span = Span {
                    start: selector_list.span.start,
                    end: block.span.end,
                };
                if selector_list.selectors.len() > 1 {
                    self.recoverable_errors.push(Error {
                        kind: ErrorKind::LessGuardOnMultipleComplexSelectors,
                        span: guard.span.clone(),
                    });
                }
                return Ok(Statement::LessConditionalQualifiedRule(
                    LessConditionalQualifiedRule {
                        selector: selector_list,
                        guard,
                        block,
                        span,
                    },
                ));
            }
            _ => {}
        }

        let block = self.parse::<SimpleBlock>()?;
        let span = Span {
            start: selector_list.span.start,
            end: block.span.end,
        };
        Ok(Statement::QualifiedRule(QualifiedRule {
            selector: selector_list,
            block,
            span,
        }))
    }

    pub(super) fn parse_maybe_hex_color_or_less_mixin_call(
        &mut self,
    ) -> PResult<ComponentValue<'s>> {
        debug_assert_eq!(self.syntax, Syntax::Less);

        let attempt = self.try_parse(|parser| {
            let hex_color = parser.parse::<HexColor>()?;
            match peek!(parser) {
                TokenWithSpan {
                    token: Token::LParen(..),
                    span,
                } => Err(Error {
                    kind: ErrorKind::TryParseError,
                    span: span.clone(),
                }),
                TokenWithSpan {
                    token: Token::LBracket(..) | Token::Dot(..) | Token::Hash(..),
                    span,
                } if hex_color.span.end == span.start => Err(Error {
                    kind: ErrorKind::TryParseError,
                    span: span.clone(),
                }),
                _ => Ok(hex_color),
            }
        });
        match attempt {
            Err(Error {
                kind: ErrorKind::TryParseError,
                ..
            }) => self.parse_less_maybe_mixin_call_or_with_lookups(),
            hex_color => hex_color.map(ComponentValue::HexColor),
        }
    }

    pub(super) fn parse_maybe_less_list(
        &mut self,
        allow_comma: bool,
    ) -> PResult<ComponentValue<'s>> {
        use util::ListSeparatorKind;

        let single_value = if allow_comma {
            self.parse_maybe_less_list(false)?
        } else if let Token::Exclamation(..) = peek!(self).token {
            self.parse().map(ComponentValue::ImportantAnnotation)?
        } else {
            self.parse_less_operation(/* allow_mixin_call */ true)?
        };

        let mut elements = vec![];
        let mut comma_spans: Option<Vec<_>> = None;
        let mut separator = ListSeparatorKind::Unknown;
        let mut end = single_value.span().end;
        loop {
            match peek!(self).token {
                Token::LBrace(..)
                | Token::RBrace(..)
                | Token::RParen(..)
                | Token::Semicolon(..)
                | Token::Colon(..)
                | Token::DotDotDot(..)
                | Token::Eof(..) => break,
                Token::Comma(..) => {
                    if !allow_comma {
                        break;
                    }
                    if separator == ListSeparatorKind::Space {
                        break;
                    } else {
                        if separator == ListSeparatorKind::Unknown {
                            separator = ListSeparatorKind::Comma;
                        }
                        let TokenWithSpan { span, .. } = bump!(self);
                        end = span.end;
                        if let Some(spans) = &mut comma_spans {
                            spans.push(span);
                        } else {
                            comma_spans = Some(vec![span]);
                        }
                    }
                }
                Token::Exclamation(..) => {
                    if let Ok(important_annotation) = self.try_parse(ImportantAnnotation::parse) {
                        if end < important_annotation.span.start
                            && separator == ListSeparatorKind::Unknown
                        {
                            separator = ListSeparatorKind::Space;
                        }
                        end = important_annotation.span.end;
                        elements.push(ComponentValue::ImportantAnnotation(important_annotation));
                    } else {
                        break;
                    }
                }
                _ => {
                    if separator == ListSeparatorKind::Unknown {
                        separator = ListSeparatorKind::Space;
                    }
                    let item = if separator == ListSeparatorKind::Comma {
                        self.parse_maybe_less_list(false)?
                    } else {
                        self.parse_less_operation(/* allow_mixin_call */ true)?
                    };
                    end = item.span().end;
                    elements.push(item);
                }
            }
        }

        if elements.is_empty() && separator != ListSeparatorKind::Comma {
            // If there is a trailing comma it can be a list,
            // though there is only one element.
            Ok(single_value)
        } else {
            debug_assert_ne!(separator, ListSeparatorKind::Unknown);

            let span = Span {
                start: single_value.span().start,
                end,
            };
            elements.insert(0, single_value);
            Ok(ComponentValue::LessList(LessList {
                elements,
                comma_spans,
                span,
            }))
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessConditions<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let when_span = match bump!(input) {
            TokenWithSpan {
                token: Token::Ident(ident),
                span,
            } if ident.raw == "when" => span,
            TokenWithSpan { span, .. } => {
                return Err(Error {
                    kind: ErrorKind::ExpectLessKeyword("when"),
                    span,
                });
            }
        };

        let first = input.parse_less_condition(true)?;
        let mut span = first.span().clone();

        let mut conditions = vec![first];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            conditions.push(input.parse_less_condition(true)?);
        }
        debug_assert_eq!(comma_spans.len() + 1, conditions.len());

        if let Some(last) = conditions.last() {
            span.end = last.span().end;
        }
        Ok(LessConditions {
            conditions,
            when_span,
            comma_spans,
            span,
        })
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
        let str: Str = input.tokenizer.scan_string_only()?.into();
        let span = Span {
            start,
            end: str.span().end,
        };
        Ok(LessEscapedStr { str, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessExtend<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let mut selector = input.parse::<ComplexSelector>()?;

        let span = selector.span.clone();
        let mut all = None;

        if let [.., complex_child, ComplexSelectorChild::Combinator(Combinator {
            kind: CombinatorKind::Descendant,
            ..
        }), ComplexSelectorChild::CompoundSelector(CompoundSelector { children, .. })] =
            &selector.children[..]
        {
            if let [SimpleSelector::Type(TypeSelector::TagName(TagNameSelector {
                name:
                    WqName {
                        name: InterpolableIdent::Literal(token_all @ Ident { raw: "all", .. }),
                        prefix: None,
                        ..
                    },
                ..
            }))] = &children[..]
            {
                all = Some(token_all.clone());
                selector.span.end = complex_child.span().end;
                selector.children.truncate(selector.children.len() - 2);
            }
        }

        Ok(LessExtend {
            selector,
            all,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessExtendList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let first = input.parse::<LessExtend>()?;
        let mut span = first.span.clone();

        let mut elements = vec![first];
        let mut comma_spans = vec![];
        while let Some((_, comma_span)) = eat!(input, Comma) {
            comma_spans.push(comma_span);
            elements.push(input.parse()?);
        }
        debug_assert_eq!(comma_spans.len() + 1, elements.len());

        if let Some(last) = elements.last() {
            span.end = last.span.end;
        }
        Ok(LessExtendList {
            elements,
            comma_spans,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessExtendRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let nesting_selector = input.parse::<NestingSelector>()?;
        if nesting_selector.suffix.is_some() {
            return Err(Error {
                kind: ErrorKind::ExpectLessExtendRule,
                span: nesting_selector.span,
            });
        }

        let pseudo_class_selector = input.parse::<PseudoClassSelector>()?;
        util::assert_no_ws_or_comment(&nesting_selector.span, &pseudo_class_selector.span)?;
        let span = Span {
            start: nesting_selector.span.start,
            end: pseudo_class_selector.span.end,
        };

        let InterpolableIdent::Literal(name_of_extend @ Ident { raw: "extend", .. }) =
            pseudo_class_selector.name
        else {
            return Err(Error {
                kind: ErrorKind::ExpectLessExtendRule,
                span,
            });
        };
        let Some(PseudoClassSelectorArg {
            kind: PseudoClassSelectorArgKind::LessExtendList(extend),
            ..
        }) = pseudo_class_selector.arg
        else {
            return Err(Error {
                kind: ErrorKind::ExpectLessExtendRule,
                span,
            });
        };

        Ok(LessExtendRule {
            nesting_selector,
            name_of_extend,
            extend,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessFormatFunction {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, span) = expect!(input, Percent);
        Ok(LessFormatFunction { span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessImportOptions<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, Span { start, .. }) = expect!(input, LParen);

        let mut names = Vec::with_capacity(1);
        let mut comma_spans = vec![];
        while let Token::Ident(crate::token::Ident {
            raw: "less" | "css" | "multiple" | "once" | "inline" | "reference" | "optional",
            ..
        }) = peek!(input).token
        {
            names.push(input.parse()?);
            if !matches!(peek!(input).token, Token::RParen(..)) {
                comma_spans.push(expect!(input, Comma).1);
            }
        }
        debug_assert!(names.len() - comma_spans.len() <= 1);

        let (_, Span { end, .. }) = expect!(input, RParen);

        Ok(LessImportOptions {
            names,
            comma_spans,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessImportPrelude<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let options = input.parse::<LessImportOptions>()?;
        let start = options.span.start;

        let href = match &peek!(input).token {
            Token::Str(..) | Token::StrTemplate(..) => input.parse().map(ImportPreludeHref::Str)?,
            _ => input.parse().map(ImportPreludeHref::Url)?,
        };
        let mut end = href.span().end;

        let media = if matches!(peek!(input).token, Token::Semicolon(..)) {
            None
        } else {
            let media = input.parse::<MediaQueryList>()?;
            end = media.span.end;
            Some(media)
        };

        Ok(LessImportPrelude {
            href,
            options,
            media,
            span: Span { start, end },
        })
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
                // '@' or '$' is consumed, so '{' left only
                let start = expect!(input, LBrace).1.start - 1;
                let (name, name_span) = expect_without_ws_or_comments!(input, Ident);

                let end = expect!(input, RBrace).1.end;
                elements.push(match input.source.as_bytes().get(start) {
                    Some(b'@') => LessInterpolatedStrElement::Variable(LessVariableInterpolation {
                        name: (name, name_span).into(),
                        span: Span { start, end },
                    }),
                    Some(b'$') => LessInterpolatedStrElement::Property(LessPropertyInterpolation {
                        name: (name, name_span).into(),
                        span: Span { start, end },
                    }),
                    _ => unreachable!(),
                });
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessListFunction {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, span) = expect!(input, Tilde);
        Ok(LessListFunction { span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessLookup<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let (_, Span { start, .. }) = expect!(input, LBracket);
        let name = if let Token::RBracket(..) = peek!(input).token {
            None
        } else {
            Some(input.parse()?)
        };
        let (_, Span { end, .. }) = expect!(input, RBracket);
        Ok(LessLookup {
            name,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessLookupName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        match peek!(input).token {
            Token::AtKeyword(..) => input.parse().map(LessLookupName::LessVariable),
            Token::At(..) => input.parse().map(LessLookupName::LessVariableVariable),
            Token::DollarVar(..) => input.parse().map(LessLookupName::LessPropertyVariable),
            _ => input.parse().map(LessLookupName::Ident),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessLookups<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let first = input.parse::<LessLookup>()?;
        let mut span = first.span.clone();

        let mut lookups = vec![first];
        while let Token::LBracket(..) = peek!(input).token {
            lookups.push(input.parse()?);
        }

        if let Some(last) = lookups.last() {
            span.end = last.span.end;
        }
        Ok(LessLookups { lookups, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessMixinCall<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let callee = input.parse::<LessMixinCallee>()?;

        let mut end = callee.span.end;
        let args = if let Some((_, lparen_span)) = eat!(input, LParen) {
            let mut semicolon_comes_at = 0;
            let mut args = vec![];
            let mut comma_spans = vec![];
            let mut semicolon_spans = vec![];
            'args: loop {
                match peek!(input).token {
                    Token::RParen(..) => {
                        let TokenWithSpan { span, .. } = bump!(input);
                        if semicolon_comes_at > 0 {
                            wrap_less_mixin_args_into_less_list(
                                &mut args,
                                mem::take(&mut comma_spans),
                                semicolon_comes_at,
                            )
                            .map_err(|kind| Error {
                                kind,
                                span: Span {
                                    // We've checked `semicolon_comes_at` must be greater than 0,
                                    // so `args` won't be empty.
                                    start: args.first().unwrap().span().start,
                                    end: args.last().unwrap().span().end,
                                },
                            })?;
                        }
                        end = span.end;
                        break;
                    }
                    Token::LBrace(..) => args.push(LessMixinArgument::Value(
                        ComponentValue::LessDetachedRuleset(input.parse()?),
                    )),
                    Token::Comma(..) => {
                        return Err(Error {
                            kind: ErrorKind::ExpectComponentValue,
                            span: bump!(input).span,
                        });
                    }
                    _ => 'maybe: {
                        let value = input.parse_maybe_less_list(/* allow_comma */ false)?;
                        let name = {
                            match value {
                                ComponentValue::LessVariable(variable) => {
                                    LessMixinParameterName::Variable(variable)
                                }
                                ComponentValue::LessPropertyVariable(property) => {
                                    LessMixinParameterName::PropertyVariable(property)
                                }
                                value => {
                                    args.push(LessMixinArgument::Value(value));
                                    break 'maybe;
                                }
                            }
                        };
                        if let Some((_, colon_span)) = eat!(input, Colon) {
                            let value = if matches!(peek!(input).token, Token::LBrace(..)) {
                                input.parse().map(ComponentValue::LessDetachedRuleset)?
                            } else {
                                input.parse_maybe_less_list(
                                    /* allow_comma */ semicolon_comes_at > 0,
                                )?
                            };
                            let span = Span {
                                start: name.span().start,
                                end: value.span().end,
                            };
                            args.push(LessMixinArgument::Named(LessMixinNamedArgument {
                                name,
                                colon_span,
                                value,
                                span,
                            }));
                        } else if let Some((_, dotdotdot_span)) = eat!(input, DotDotDot) {
                            let span = Span {
                                start: name.span().start,
                                end: dotdotdot_span.end,
                            };
                            args.push(LessMixinArgument::Variadic(LessMixinVariadicArgument {
                                name,
                                span,
                            }));
                            if let Some((_, semicolon_span)) = eat!(input, Semicolon) {
                                semicolon_spans.push(semicolon_span);
                            };
                            end = expect!(input, RParen).1.end;
                            break 'args;
                        } else {
                            args.push(LessMixinArgument::Value(match name {
                                LessMixinParameterName::Variable(variable) => {
                                    ComponentValue::LessVariable(variable)
                                }
                                LessMixinParameterName::PropertyVariable(property_variable) => {
                                    ComponentValue::LessPropertyVariable(property_variable)
                                }
                            }));
                        }
                    }
                };

                match peek!(input).token {
                    Token::RParen(..) => {}
                    Token::Comma(..) => {
                        comma_spans.push(bump!(input).span);
                    }
                    Token::Semicolon(..) => {
                        let TokenWithSpan { span, .. } = bump!(input);
                        wrap_less_mixin_args_into_less_list(
                            &mut args,
                            mem::take(&mut comma_spans),
                            semicolon_comes_at,
                        )
                        .map_err(|kind| Error {
                            kind,
                            span: span.clone(),
                        })?;
                        semicolon_comes_at = args.len();
                        semicolon_spans.push(span);
                    }
                    _ => {
                        let TokenWithSpan { token, span } = bump!(input);
                        use crate::{token::RParen, tokenizer::TokenSymbol};
                        return Err(Error {
                            kind: ErrorKind::Unexpected(RParen::symbol(), token.symbol()),
                            span,
                        });
                    }
                }
            }
            let is_comma_separated = semicolon_spans.is_empty();
            let separator_spans = if semicolon_spans.is_empty() {
                comma_spans
            } else {
                semicolon_spans
            };
            debug_assert!(args.len() - separator_spans.len() <= 1);
            Some(LessMixinArguments {
                args,
                is_comma_separated,
                separator_spans,
                span: Span {
                    start: lparen_span.start,
                    end,
                },
            })
        } else {
            None
        };

        let important = if !matches!(
            input.state.qualified_rule_ctx,
            Some(QualifiedRuleContext::DeclarationValue)
        ) && matches!(peek!(input).token, Token::Exclamation(..))
        {
            input.parse::<ImportantAnnotation>().map(Some)?
        } else {
            None
        };

        let span = Span {
            start: callee.span.start,
            end: important
                .as_ref()
                .map(|important| important.span.end)
                .unwrap_or(end),
        };
        Ok(LessMixinCall {
            callee,
            args,
            important,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessMixinCallee<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first_name = input.parse::<LessMixinName>()?;
        let mut span = first_name.span().clone();

        let mut children = vec![LessMixinCalleeChild {
            name: first_name,
            combinator: None,
            span: span.clone(),
        }];
        loop {
            let combinator = eat!(input, GreaterThan).map(|(_, span)| Combinator {
                kind: CombinatorKind::Child,
                span,
            });
            if let Token::Dot(..) | Token::Hash(..) = peek!(input).token {
                let name = input.parse::<LessMixinName>()?;
                let name_span = name.span();
                let span = Span {
                    start: combinator
                        .as_ref()
                        .map(|combinator| combinator.span.start)
                        .unwrap_or(name_span.start),
                    end: name_span.end,
                };
                children.push(LessMixinCalleeChild {
                    name,
                    combinator,
                    span,
                });
            } else {
                break;
            }
        }

        if let Some(last) = children.last() {
            span.end = last.span.end;
        }
        Ok(LessMixinCallee { children, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessMixinDefinition<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let name = input.parse::<LessMixinName>()?;

        let (_, lparen_span) = expect!(input, LParen);
        let rparen_span;
        let mut semicolon_comes_at = 0;
        let mut params = vec![];
        let mut comma_spans = vec![];
        let mut semicolon_spans = vec![];
        'params: loop {
            match peek!(input).token {
                Token::RParen(..) => {
                    rparen_span = bump!(input).span;
                    break;
                }
                Token::DotDotDot(..) => {
                    let TokenWithSpan { span, .. } = bump!(input);
                    params.push(LessMixinParameter::Variadic(LessMixinVariadicParameter {
                        name: None,
                        span,
                    }));
                    eat!(input, Semicolon);
                    (_, rparen_span) = expect!(input, RParen);
                    break;
                }
                Token::Comma(..) => {
                    return Err(Error {
                        kind: ErrorKind::ExpectComponentValue,
                        span: bump!(input).span,
                    });
                }
                _ => 'maybe: {
                    let value = input
                        .with_state(ParserState {
                            less_ctx: input.state.less_ctx | LESS_CTX_ALLOW_DIV,
                            ..input.state.clone()
                        })
                        .parse::<ComponentValue>()?;
                    let name = {
                        match value {
                            ComponentValue::LessVariable(variable) => {
                                LessMixinParameterName::Variable(variable)
                            }
                            ComponentValue::LessPropertyVariable(property) => {
                                LessMixinParameterName::PropertyVariable(property)
                            }
                            value => {
                                let span = value.span().clone();
                                params.push(LessMixinParameter::Unnamed(
                                    LessMixinUnnamedParameter { value, span },
                                ));
                                break 'maybe;
                            }
                        }
                    };
                    let name_span = name.span();
                    if let Some((_, colon_span)) = eat!(input, Colon) {
                        let value = if matches!(peek!(input).token, Token::LBrace(..)) {
                            input.parse().map(ComponentValue::LessDetachedRuleset)?
                        } else {
                            input
                                .with_state(ParserState {
                                    less_ctx: input.state.less_ctx | LESS_CTX_ALLOW_DIV,
                                    ..input.state.clone()
                                })
                                .parse_maybe_less_list(/* allow_comma */ false)?
                        };
                        let end = value.span().end;
                        let default_value = {
                            let span = Span {
                                start: colon_span.start,
                                end,
                            };
                            LessMixinNamedParameterDefaultValue {
                                colon_span,
                                value,
                                span,
                            }
                        };
                        let span = Span {
                            start: name_span.start,
                            end,
                        };
                        params.push(LessMixinParameter::Named(LessMixinNamedParameter {
                            name,
                            value: Some(default_value),
                            span,
                        }));
                    } else if let Some((_, Span { end, .. })) = eat!(input, DotDotDot) {
                        let span = Span {
                            start: name_span.start,
                            end,
                        };
                        params.push(LessMixinParameter::Variadic(LessMixinVariadicParameter {
                            name: Some(name),
                            span,
                        }));
                        if let Some((_, semicolon_span)) = eat!(input, Semicolon) {
                            semicolon_spans.push(semicolon_span);
                        };
                        (_, rparen_span) = expect!(input, RParen);
                        break 'params;
                    } else {
                        let span = name_span.clone();
                        params.push(LessMixinParameter::Named(LessMixinNamedParameter {
                            name,
                            value: None,
                            span,
                        }));
                    }
                }
            }

            match bump!(input) {
                TokenWithSpan {
                    token: Token::RParen(..),
                    span,
                } => {
                    if semicolon_comes_at > 0 {
                        wrap_less_mixin_params_into_less_list(
                            &mut params,
                            mem::take(&mut comma_spans),
                            semicolon_comes_at,
                        )
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
                    rparen_span = span;
                    break;
                }
                TokenWithSpan {
                    token: Token::Comma(..),
                    span,
                } => {
                    comma_spans.push(span);
                }
                TokenWithSpan {
                    token: Token::Semicolon(..),
                    span,
                } => {
                    wrap_less_mixin_params_into_less_list(
                        &mut params,
                        mem::take(&mut comma_spans),
                        semicolon_comes_at,
                    )
                    .map_err(|kind| Error {
                        kind,
                        span: span.clone(),
                    })?;
                    semicolon_comes_at = params.len();
                    semicolon_spans.push(span);
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
        let is_comma_separated = semicolon_spans.is_empty();
        let separator_spans = if semicolon_spans.is_empty() {
            comma_spans
        } else {
            semicolon_spans
        };
        debug_assert!(params.len() - separator_spans.len() <= 1);
        let params = LessMixinParameters {
            params,
            is_comma_separated,
            separator_spans,
            span: Span {
                start: lparen_span.start,
                end: rparen_span.end,
            },
        };

        let guard = match &peek!(input).token {
            Token::Ident(ident) if ident.raw == "when" => Some(input.parse()?),
            _ => None,
        };

        let block = input
            .with_state(ParserState {
                less_ctx: input.state.less_ctx | LESS_CTX_ALLOW_KEYFRAME_BLOCK,
                ..input.state.clone()
            })
            .parse::<SimpleBlock>()?;

        let span = Span {
            start: name.span().start,
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessMixinName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match bump!(input) {
            TokenWithSpan {
                token: Token::Dot(..),
                span: dot_span,
            } => {
                let ident: Ident = expect_without_ws_or_comments!(input, Ident).into();
                let span = Span {
                    start: dot_span.start,
                    end: ident.span.end,
                };
                Ok(LessMixinName::ClassSelector(ClassSelector {
                    name: InterpolableIdent::Literal(ident),
                    span,
                }))
            }
            TokenWithSpan {
                token: Token::Hash(hash),
                span,
            } => {
                let raw = hash.raw;
                if raw.starts_with(|c: char| c.is_ascii_digit())
                    || matches!(raw.as_bytes(), [b'-', c, ..] if *c != b'-')
                {
                    input.recoverable_errors.push(Error {
                        kind: ErrorKind::InvalidIdSelectorName,
                        span: span.clone(),
                    });
                }
                let name = if hash.escaped {
                    util::handle_escape(raw)
                } else {
                    Cow::from(raw)
                };
                let name_span = Span {
                    start: span.start + 1,
                    end: span.end,
                };
                Ok(LessMixinName::IdSelector(IdSelector {
                    name: InterpolableIdent::Literal(Ident {
                        name,
                        raw,
                        span: name_span,
                    }),
                    span,
                }))
            }
            TokenWithSpan { token, span } => {
                use crate::{
                    token::{Dot, Hash},
                    tokenizer::TokenSymbol,
                };
                Err(Error {
                    kind: ErrorKind::ExpectOneOf(
                        vec![Dot::symbol(), Hash::symbol()],
                        token.symbol(),
                    ),
                    span,
                })
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessMixinParameterName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if matches!(peek!(input).token, Token::AtKeyword(..)) {
            input.parse().map(LessMixinParameterName::Variable)
        } else {
            input.parse().map(LessMixinParameterName::PropertyVariable)
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessNamespaceValue<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let callee = input.parse::<LessNamespaceValueCallee>()?;
        let callee_span = callee.span();

        let lookups = input.parse::<LessLookups>()?;
        util::assert_no_ws_or_comment(callee_span, &lookups.span)?;

        let span = Span {
            start: callee_span.start,
            end: lookups.span.end,
        };
        Ok(LessNamespaceValue {
            callee,
            lookups,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessNamespaceValueCallee<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if matches!(peek!(input).token, Token::AtKeyword(..)) {
            input.parse().map(LessNamespaceValueCallee::LessVariable)
        } else {
            input.parse().map(LessNamespaceValueCallee::LessMixinCall)
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessNegativeValue<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, minus_span) = expect!(input, Minus);
        let value = match peek!(input) {
            TokenWithSpan {
                token: Token::AtKeyword(..) | Token::At(..) | Token::DollarVar(..),
                span,
            } if minus_span.end == span.start => Box::new(input.parse_component_value_atom()?),
            TokenWithSpan {
                token: Token::LParen(..),
                span,
            } if minus_span.end == span.start => {
                Box::new(ComponentValue::LessParenthesizedOperation(
                    input.parse_less_parenthesized_operation(/* allow_mixin_call */ true)?,
                ))
            }
            TokenWithSpan { token, span } => {
                use crate::{
                    token::{AtKeyword, DollarVar, LParen},
                    tokenizer::TokenSymbol,
                };
                return Err(Error {
                    kind: ErrorKind::ExpectOneOf(
                        vec![AtKeyword::symbol(), DollarVar::symbol(), LParen::symbol()],
                        token.symbol(),
                    ),
                    span: span.clone(),
                });
            }
        };

        let span = Span {
            start: minus_span.start,
            end: value.span().end,
        };
        Ok(LessNegativeValue { value, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessPercentKeyword {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, span) = expect!(input, Percent);
        Ok(LessPercentKeyword { span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessPlugin<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let mut start = None;

        let args = if let Some((_, span)) = eat!(input, LParen) {
            start = Some(span.start);
            let args = input.parse_tokens_in_parens()?;
            expect!(input, RParen);
            Some(args)
        } else {
            None
        };

        let path = input.parse::<LessPluginPath>()?;
        let path_span = path.span();

        let span = Span {
            start: start.unwrap_or(path_span.start),
            end: path_span.end,
        };
        Ok(LessPlugin { path, args, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessPluginPath<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if let Token::Str(..) = peek!(input).token {
            input.parse().map(LessPluginPath::Str)
        } else {
            input.parse().map(LessPluginPath::Url)
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessPropertyInterpolation<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (dollar_lbrace_var, span) = expect!(input, DollarLBraceVar);
        Ok(LessPropertyInterpolation {
            name: (
                dollar_lbrace_var.ident,
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableCall<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let variable = input.parse::<LessVariable>()?;
        expect_without_ws_or_comments!(input, LParen);
        let (_, Span { end, .. }) = expect!(input, RParen);

        let span = Span {
            start: variable.span.start,
            end,
        };
        Ok(LessVariableCall { variable, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LessVariableDeclaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        debug_assert_eq!(input.syntax, Syntax::Less);

        let name = input.parse::<LessVariable>()?;
        let (_, colon_span) = expect!(input, Colon);
        let value = if matches!(peek!(input).token, Token::LBrace(..)) {
            ComponentValue::LessDetachedRuleset(input.parse()?)
        } else {
            input
                .with_state(ParserState {
                    less_ctx: input.state.less_ctx | LESS_CTX_ALLOW_DIV,
                    ..input.state.clone()
                })
                .parse_maybe_less_list(/* allow_comma */ true)?
        };

        let span = Span {
            start: name.span.start,
            end: value.span().end,
        };
        Ok(LessVariableDeclaration {
            name,
            colon_span,
            value,
            span,
        })
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

fn wrap_less_mixin_params_into_less_list(
    params: &mut Vec<LessMixinParameter<'_>>,
    comma_spans: Vec<Span>,
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
                if let LessMixinParameter::Unnamed(LessMixinUnnamedParameter { value, .. }) = param
                {
                    Ok(value)
                } else {
                    // reject code like this:
                    // .mixin(@a: 5, @b: 6; @c: 7) {}
                    // .mixin(@a: 5; @b: 6, @c: 7) {}
                    Err(ErrorKind::MixedDelimiterKindInLessMixin)
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        debug_assert!(elements.len() - comma_spans.len() <= 1);
        params.push(LessMixinParameter::Unnamed(LessMixinUnnamedParameter {
            value: ComponentValue::LessList(LessList {
                elements,
                comma_spans: Some(comma_spans),
                span: span.clone(),
            }),
            span,
        }));
    }
    Ok(())
}

fn wrap_less_mixin_args_into_less_list(
    args: &mut Vec<LessMixinArgument<'_>>,
    comma_spans: Vec<Span>,
    index: usize,
) -> Result<(), ErrorKind> {
    if let [first, .., last] = &args[index..] {
        let span = Span {
            start: first.span().start,
            end: last.span().end,
        };
        let elements = args
            .drain(index..)
            .map(|param| {
                if let LessMixinArgument::Value(value) = param {
                    Ok(value)
                } else {
                    // reject code like this:
                    // .mixin(@a: 5, @b: 6; @c: 7) {}
                    // .mixin(@a: 5; @b: 6, @c: 7) {}
                    Err(ErrorKind::MixedDelimiterKindInLessMixin)
                }
            })
            .collect::<Result<Vec<_>, _>>()?;
        debug_assert!(elements.len() - comma_spans.len() <= 1);
        args.push(LessMixinArgument::Value(ComponentValue::LessList(
            LessList {
                elements,
                comma_spans: Some(comma_spans),
                span,
            },
        )));
    }
    Ok(())
}

fn can_be_division_operand(left: &ComponentValue) -> bool {
    matches!(
        left,
        ComponentValue::LessVariable(..)
            | ComponentValue::LessPropertyVariable(..)
            | ComponentValue::LessBinaryOperation(..)
            | ComponentValue::LessParenthesizedOperation(..)
    )
}
