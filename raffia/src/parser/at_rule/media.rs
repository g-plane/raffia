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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaAnd<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse::<Ident>()?;
        if ident.name.eq_ignore_ascii_case("and") {
            let media_in_parens = input.parse::<MediaInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: media_in_parens.span().end,
            };
            Ok(MediaAnd {
                ident,
                media_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectMediaAnd,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaFeature<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let left = input.parse_media_feature_value()?;
        if let ComponentValue::InterpolableIdent(ident) = left {
            match input.tokenizer.peek()? {
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
            }
        } else {
            input.parse_media_feature_range_or_range_interval(left)
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaFeatureComparison {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.bump()? {
            Token::LessThan(token) => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::LessThan,
                span: token.span,
            }),
            Token::LessThanEqual(token) => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::LessThanEqual,
                span: token.span,
            }),
            Token::GreaterThan(token) => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::GreaterThan,
                span: token.span,
            }),
            Token::GreaterThanEqual(token) => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::GreaterThanEqual,
                span: token.span,
            }),
            Token::Equal(token) => Ok(MediaFeatureComparison {
                kind: MediaFeatureComparisonKind::Equal,
                span: token.span,
            }),
            token => Err(Error {
                kind: ErrorKind::ExpectMediaFeatureComparison,
                span: token.span().clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaInParens<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        expect!(input, LParen);
        let media_in_parens = if let Some(media_condition) =
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
        let ident = input.parse::<Ident>()?;
        if ident.name.eq_ignore_ascii_case("not") {
            let media_in_parens = input.parse::<MediaInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: media_in_parens.span().end,
            };
            Ok(MediaNot {
                ident,
                media_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectMediaNot,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaOr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse::<Ident>()?;
        if ident.name.eq_ignore_ascii_case("or") {
            let media_in_parens = input.parse::<MediaInParens>()?;
            let span = Span {
                start: ident.span.start,
                end: media_in_parens.span().end,
            };
            Ok(MediaOr {
                ident,
                media_in_parens,
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectMediaOr,
                span: ident.span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaQuery<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if let Some(condition_only) =
            input.try_parse(|parser| parser.parse_media_condition(/* allow_or */ true))
        {
            Ok(MediaQuery::ConditionOnly(condition_only))
        } else {
            input.parse().map(MediaQuery::WithType)
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediqQueryList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<MediaQuery>()?;
        let mut span = first.span().clone();

        let mut queries = vec![first];
        while eat!(input, Comma).is_some() {
            queries.push(input.parse()?);
        }

        if let Some(last) = queries.last() {
            span.end = last.span().end;
        }
        Ok(MediqQueryList { queries, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for MediaQueryWithType<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let modifier = match input.tokenizer.peek()? {
            Token::Ident(ident)
                if ident.name.eq_ignore_ascii_case("not")
                    || ident.name.eq_ignore_ascii_case("only") =>
            {
                Some(input.parse::<Ident>()?)
            }
            _ => None,
        };
        let media_type = input.parse::<InterpolableIdent>()?;
        let condition = match input.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => {
                input.tokenizer.bump()?;
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
        match self.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("not") => {
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
                match self.tokenizer.peek()? {
                    Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => loop {
                        conditions.push(MediaConditionKind::And(self.parse()?));
                        match self.tokenizer.peek()? {
                            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => {}
                            _ => break,
                        }
                    },
                    Token::Ident(ident) if allow_or && ident.name.eq_ignore_ascii_case("or") => {
                        loop {
                            conditions.push(MediaConditionKind::Or(self.parse()?));
                            match self.tokenizer.peek()? {
                                Token::Ident(ident) if ident.name.eq_ignore_ascii_case("or") => {}
                                _ => break,
                            }
                        }
                    }
                    _ => {}
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
            match self.tokenizer.peek()? {
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
        } else if !matches!(left, ComponentValue::InterpolableIdent(..))
            && !matches!(name_or_right, ComponentValue::InterpolableIdent(..))
        {
            // this should be recoverable
            Err(Error {
                kind: ErrorKind::ExpectMediaFeatureName,
                span: name_or_right.span().clone(),
            })
        } else {
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
        match self.parse_component_value_atom()? {
            ComponentValue::Number(number) => match self.tokenizer.peek()? {
                Token::Solidus(..) if number.value >= 0.0 => {
                    self.parse_ratio(number).map(ComponentValue::Ratio)
                }
                _ => Ok(ComponentValue::Number(number)),
            },
            value => Ok(value),
        }
    }
}
