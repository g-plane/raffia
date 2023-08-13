use super::Parser;
use crate::{
    ast::*,
    bump, eat,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    Parse, Syntax,
};
use smallvec::{smallvec, SmallVec};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaAnd<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let keyword = input.parse::<Ident>()?;
        if keyword.name.eq_ignore_ascii_case("and") {
            let media_in_parens = input.parse::<MediaInParens>()?;
            let span = Span {
                start: keyword.span.start,
                end: media_in_parens.span().end,
            };
            Ok(MediaAnd {
                keyword,
                media_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectMediaAnd,
                span: keyword.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaFeature<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.parse_media_feature_value()? {
            ComponentValue::InterpolableIdent(ident) => match &peek!(input).token {
                Token::Colon(..) => input
                    .parse_media_feature_plain(ident)
                    .map(MediaFeature::Plain),
                Token::LessThan(..)
                | Token::LessThanEqual(..)
                | Token::GreaterThan(..)
                | Token::GreaterThanEqual(..)
                | Token::Equal(..) => input.parse_media_feature_range_or_range_interval(
                    ComponentValue::InterpolableIdent(ident),
                ),
                _ => {
                    let span = ident.span().clone();
                    Ok(MediaFeature::Boolean(MediaFeatureBoolean {
                        name: MediaFeatureName::Ident(ident),
                        span,
                    }))
                }
            },
            ComponentValue::SassVariable(variable) => {
                let span = variable.span.clone();
                Ok(MediaFeature::Boolean(MediaFeatureBoolean {
                    name: MediaFeatureName::SassVariable(variable),
                    span,
                }))
            }
            value => input.parse_media_feature_range_or_range_interval(value),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaFeatureComparison {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match bump!(input) {
            TokenWithSpan {
                token: Token::LessThan(..),
                span,
            } => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::LessThan,
                span,
            }),
            TokenWithSpan {
                token: Token::LessThanEqual(..),
                span,
            } => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::LessThanOrEqual,
                span,
            }),
            TokenWithSpan {
                token: Token::GreaterThan(..),
                span,
            } => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::GreaterThan,
                span,
            }),
            TokenWithSpan {
                token: Token::GreaterThanEqual(..),
                span,
            } => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::GreaterThanOrEqual,
                span,
            }),
            TokenWithSpan {
                token: Token::Equal(..),
                span,
            } => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::Equal,
                span,
            }),
            TokenWithSpan { span, .. } => Err(Error {
                kind: ErrorKind::ExpectMediaFeatureComparison,
                span,
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaInParens<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        expect!(input, LParen);
        let media_in_parens = if let Ok(media_condition) =
            input.try_parse(|parser| parser.parse_media_condition(/* allow_or */ true))
        {
            MediaInParens::MediaCondition(media_condition)
        } else {
            MediaInParens::MediaFeature(Box::new(input.parse()?))
        };
        expect!(input, RParen);
        Ok(media_in_parens)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaNot<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let keyword = input.parse::<Ident>()?;
        if keyword.name.eq_ignore_ascii_case("not") {
            let media_in_parens = input.parse::<MediaInParens>()?;
            let span = Span {
                start: keyword.span.start,
                end: media_in_parens.span().end,
            };
            Ok(MediaNot {
                keyword,
                media_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectMediaNot,
                span: keyword.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaOr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let keyword = input.parse::<Ident>()?;
        if keyword.name.eq_ignore_ascii_case("or") {
            let media_in_parens = input.parse::<MediaInParens>()?;
            let span = Span {
                start: keyword.span.start,
                end: media_in_parens.span().end,
            };
            Ok(MediaOr {
                keyword,
                media_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectMediaOr,
                span: keyword.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaQuery<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if let Ok(condition_only) =
            input.try_parse(|parser| parser.parse_media_condition(/* allow_or */ true))
        {
            Ok(MediaQuery::ConditionOnly(condition_only))
        } else {
            match peek!(input).token {
                Token::AtKeyword(..) if input.syntax == Syntax::Less => {
                    input.parse().map(MediaQuery::LessVariable)
                }
                _ => input.parse().map(MediaQuery::WithType),
            }
        }
    }
}

// https://www.w3.org/TR/mediaqueries-4/#mq-syntax
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaQueryList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<MediaQuery>()?;
        let mut span = first.span().clone();

        let mut queries: SmallVec<[MediaQuery; 1]> = smallvec![first];
        while eat!(input, Comma).is_some() {
            queries.push(input.parse()?);
        }

        // SAFETY: it has at least one element.
        span.end = unsafe {
            let index = queries.len() - 1;
            queries.get_unchecked(index).span().end
        };
        Ok(MediaQueryList { queries, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaQueryWithType<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let modifier = if let Token::Ident(ident) = &peek!(input).token {
            let name = ident.name();
            if name.eq_ignore_ascii_case("not") || name.eq_ignore_ascii_case("only") {
                Some(input.parse::<Ident>()?)
            } else {
                None
            }
        } else {
            None
        };
        let media_type = input.parse::<InterpolableIdent>()?;
        if let InterpolableIdent::Literal(Ident { name, span, .. }) = &media_type {
            if name.eq_ignore_ascii_case("only")
                || name.eq_ignore_ascii_case("not")
                || name.eq_ignore_ascii_case("and")
                || name.eq_ignore_ascii_case("or")
                || name.eq_ignore_ascii_case("layer")
            {
                input.recoverable_errors.push(Error {
                    kind: ErrorKind::MediaTypeKeywordDisallowed(name.to_string()),
                    span: span.clone(),
                });
            }
        }
        let condition = match &peek!(input).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("and") => {
                bump!(input);
                input
                    .parse_media_condition(/* allow_or */ false)
                    .map(Some)?
            }
            _ => None,
        };

        let mut span = media_type.span().clone();
        if let Some(modifier) = &modifier {
            span.start = modifier.span.start;
        }
        if let Some(last_condition) = condition
            .as_ref()
            .and_then(|condition| condition.conditions.last())
        {
            span.end = last_condition.span().end;
        }
        Ok(MediaQueryWithType {
            modifier,
            media_type,
            condition,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn parse_media_condition(&mut self, allow_or: bool) -> PResult<MediaCondition<'s>> {
        match &peek!(self).token {
            Token::Ident(ident) if ident.name().eq_ignore_ascii_case("not") => {
                let media_not = self.parse::<MediaNot>()?;
                let span = media_not.span.clone();
                Ok(MediaCondition {
                    conditions: vec![MediaConditionKind::Not(media_not)],
                    span,
                })
            }
            _ => {
                let first = self.parse::<MediaInParens>()?;
                let mut span = first.span().clone();
                let mut conditions = vec![MediaConditionKind::MediaInParens(first)];
                if let Token::Ident(ident) = &peek!(self).token {
                    let name = ident.name();
                    if name.eq_ignore_ascii_case("and") {
                        loop {
                            conditions.push(MediaConditionKind::And(self.parse()?));
                            match &peek!(self).token {
                                Token::Ident(ident) if ident.name().eq_ignore_ascii_case("and") => {
                                }
                                _ => break,
                            }
                        }
                    } else if allow_or && name.eq_ignore_ascii_case("or") {
                        loop {
                            conditions.push(MediaConditionKind::Or(self.parse()?));
                            match &peek!(self).token {
                                Token::Ident(ident) if ident.name().eq_ignore_ascii_case("or") => {}
                                _ => break,
                            }
                        }
                    }
                }

                if let Some(last) = conditions.last() {
                    span.end = last.span().end;
                }
                Ok(MediaCondition { conditions, span })
            }
        }
    }

    fn parse_media_feature_plain(
        &mut self,
        ident: InterpolableIdent<'s>,
    ) -> PResult<MediaFeaturePlain<'s>> {
        expect!(self, Colon);
        let value = self.parse_media_feature_value()?;
        let span = Span {
            start: ident.span().start,
            end: value.span().end,
        };
        Ok(MediaFeaturePlain {
            name: MediaFeatureName::Ident(ident),
            value,
            span,
        })
    }

    fn parse_media_feature_range_or_range_interval(
        &mut self,
        left: ComponentValue<'s>,
    ) -> PResult<MediaFeature<'s>> {
        let comparison = self.parse()?;
        let name_or_right = self.parse_media_feature_value()?;
        if let ComponentValue::InterpolableIdent(ident) = name_or_right {
            match &peek!(self).token {
                Token::LessThan(..)
                | Token::LessThanEqual(..)
                | Token::GreaterThan(..)
                | Token::GreaterThanEqual(..)
                | Token::Equal(..) => {
                    let right_comparison = self.parse()?;
                    let right = self.parse_media_feature_value()?;
                    let span = Span {
                        start: left.span().start,
                        end: right.span().end,
                    };
                    Ok(MediaFeature::RangeInterval(MediaFeatureRangeInterval {
                        left,
                        left_comparison: comparison,
                        name: MediaFeatureName::Ident(ident),
                        right_comparison,
                        right,
                        span,
                    }))
                }
                _ => {
                    let span = Span {
                        start: left.span().start,
                        end: ident.span().end,
                    };
                    Ok(MediaFeature::Range(MediaFeatureRange {
                        left,
                        comparison,
                        right: ComponentValue::InterpolableIdent(ident),
                        span,
                    }))
                }
            }
        } else {
            if !matches!(left, ComponentValue::InterpolableIdent(..))
                && !matches!(name_or_right, ComponentValue::InterpolableIdent(..))
            {
                self.recoverable_errors.push(Error {
                    kind: ErrorKind::ExpectMediaFeatureName,
                    span: name_or_right.span().clone(),
                });
            }
            let span = Span {
                start: left.span().start,
                end: name_or_right.span().end,
            };
            Ok(MediaFeature::Range(MediaFeatureRange {
                left,
                comparison,
                right: name_or_right,
                span,
            }))
        }
    }

    fn parse_media_feature_value(&mut self) -> PResult<ComponentValue<'s>> {
        match self.parse()? {
            ComponentValue::Number(number)
                if number.value >= 0.0 && matches!(peek!(self).token, Token::Solidus(..)) =>
            {
                self.parse_ratio(number).map(ComponentValue::Ratio)
            }
            value => Ok(value),
        }
    }
}
