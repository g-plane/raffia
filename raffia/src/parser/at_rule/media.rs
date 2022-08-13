use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::Token,
};

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn parse_media_and(&mut self) -> PResult<MediaAnd<'s>> {
        let ident = self.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("and") {
            let media_in_parens = self.parse_media_in_parens()?;
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

    fn parse_media_condition(&mut self, allow_or: bool) -> PResult<MediaCondition<'s>> {
        match self.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("not") => {
                let media_not = self.parse_media_not()?;
                let span = media_not.span.clone();
                Ok(MediaCondition {
                    conditions: vec![MediaConditionKind::Not(media_not)],
                    span,
                })
            }
            _ => {
                let first = self.parse_media_in_parens()?;
                let mut span = first.span().clone();
                let mut conditions = vec![MediaConditionKind::MediaInParens(first)];
                match self.tokenizer.peek()? {
                    Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => loop {
                        conditions.push(MediaConditionKind::And(self.parse_media_and()?));
                        match self.tokenizer.peek()? {
                            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => {}
                            _ => break,
                        }
                    },
                    Token::Ident(ident) if allow_or && ident.name.eq_ignore_ascii_case("or") => {
                        loop {
                            conditions.push(MediaConditionKind::Or(self.parse_media_or()?));
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

    pub(super) fn parse_media_feature(&mut self) -> PResult<MediaFeature<'s>> {
        let left = self.parse_media_feature_value()?;
        if let ComponentValue::InterpolableIdent(ident) = left {
            match self.tokenizer.peek()? {
                Token::Colon(..) => self
                    .parse_media_feature_plain(ident)
                    .map(MediaFeature::Plain),
                Token::LessThan(..)
                | Token::LessThanEqual(..)
                | Token::GreaterThan(..)
                | Token::GreaterThanEqual(..)
                | Token::Equal(..) => self.parse_media_feature_range_or_range_interval(
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
            self.parse_media_feature_range_or_range_interval(left)
        }
    }

    fn parse_media_feature_comparison(&mut self) -> PResult<MediaFeatureComparison> {
        match self.tokenizer.bump()? {
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
        let comparison = self.parse_media_feature_comparison()?;
        let name_or_right = self.parse_media_feature_value()?;
        if let ComponentValue::InterpolableIdent(ident) = name_or_right {
            match self.tokenizer.peek()? {
                Token::LessThan(..)
                | Token::LessThanEqual(..)
                | Token::GreaterThan(..)
                | Token::GreaterThanEqual(..)
                | Token::Equal(..) => {
                    let right_comparison = self.parse_media_feature_comparison()?;
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

    fn parse_media_in_parens(&mut self) -> PResult<MediaInParens<'s>> {
        expect!(self, LParen);
        let media_in_parens = if let Some(media_condition) =
            self.try_parse(|parser| parser.parse_media_condition(/* allow_or */ true))
        {
            MediaInParens::MediaCondition(media_condition)
        } else {
            MediaInParens::MediaFeature(Box::new(self.parse_media_feature()?))
        };
        expect!(self, RParen);
        Ok(media_in_parens)
    }

    fn parse_media_not(&mut self) -> PResult<MediaNot<'s>> {
        let ident = self.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("not") {
            let media_in_parens = self.parse_media_in_parens()?;
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

    fn parse_media_or(&mut self) -> PResult<MediaOr<'s>> {
        let ident = self.parse_ident()?;
        if ident.name.eq_ignore_ascii_case("or") {
            let media_in_parens = self.parse_media_in_parens()?;
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

    pub(in crate::parser) fn parse_media_query(&mut self) -> PResult<MediaQuery<'s>> {
        if let Some(condition_only) =
            self.try_parse(|parser| parser.parse_media_condition(/* allow_or */ true))
        {
            Ok(MediaQuery::ConditionOnly(condition_only))
        } else {
            self.parse_media_query_with_type().map(MediaQuery::WithType)
        }
    }

    pub(in crate::parser) fn parse_media_query_list(&mut self) -> PResult<MediqQueryList<'s>> {
        let first = self.parse_media_query()?;
        let mut span = first.span().clone();

        let mut queries = vec![first];
        while eat!(self, Comma).is_some() {
            queries.push(self.parse_media_query()?);
        }

        if let Some(last) = queries.last() {
            span.end = last.span().end;
        }
        Ok(MediqQueryList { queries, span })
    }

    fn parse_media_query_with_type(&mut self) -> PResult<MediaQueryWithType<'s>> {
        let modifier = match self.tokenizer.peek()? {
            Token::Ident(ident)
                if ident.name.eq_ignore_ascii_case("not")
                    || ident.name.eq_ignore_ascii_case("only") =>
            {
                Some(self.parse_ident()?)
            }
            _ => None,
        };
        let media_type = self.parse_interpolable_ident()?;
        let condition = match self.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("and") => {
                self.tokenizer.bump()?;
                self.parse_media_condition(/* allow_or */ false).map(Some)?
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
