use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    expect,
    pos::{Span, Spanned},
    tokenizer::{token, Token},
    Syntax,
};
use raffia_derive::Spanned;

impl<'a> Parser<'a> {
    fn assert_no_ws_or_comment(&self, left: &Span, right: &Span) -> PResult<()> {
        debug_assert!(left.end <= right.start);
        if left.end == right.start {
            Ok(())
        } else {
            Err(Error {
                kind: ErrorKind::UnexpectedWhitespace,
                span: Span {
                    start: left.end,
                    end: right.start,
                },
            })
        }
    }

    fn parse_attribute_selector(&mut self) -> PResult<AttributeSelector<'a>> {
        let l_bracket = expect!(self, LBracket);

        let name = match self.tokenizer.bump()? {
            Token::Ident(..) | Token::HashLBrace(..) => {
                let ident = self.parse_interpolable_ident()?;
                let ident_span = ident.span();
                if let Some(bar_token) = eat!(self, Bar) {
                    self.assert_no_ws_or_comment(ident_span, &bar_token.span)?;

                    let name = self.parse_interpolable_ident()?;
                    let name_span = name.span();
                    self.assert_no_ws_or_comment(&bar_token.span, name_span)?;

                    let start = ident_span.start;
                    let end = name_span.end;
                    WqName {
                        name,
                        prefix: Some(NsPrefix {
                            kind: Some(NsPrefixKind::Ident(ident)),
                            span: Span {
                                start,
                                end: bar_token.span.end,
                            },
                        }),
                        span: Span { start, end },
                    }
                } else {
                    let span = ident_span.clone();
                    WqName {
                        name: ident,
                        prefix: None,
                        span,
                    }
                }
            }
            Token::Asterisk(asterisk) => {
                let asterisk_span = asterisk.span;
                let bar_token = expect!(self, Bar);
                let name = self.parse_interpolable_ident()?;

                let start = asterisk_span.start;
                let end = name.span().end;
                WqName {
                    name,
                    prefix: Some(NsPrefix {
                        kind: Some(NsPrefixKind::Universal(NsPrefixUniversal {
                            span: asterisk_span,
                        })),
                        span: Span {
                            start,
                            end: bar_token.span.end,
                        },
                    }),
                    span: Span { start, end },
                }
            }
            Token::Bar(bar_token) => {
                let name = self.parse_interpolable_ident()?;

                let start = bar_token.span.start;
                let end = name.span().end;
                WqName {
                    name,
                    prefix: Some(NsPrefix {
                        kind: None,
                        span: Span {
                            start,
                            end: bar_token.span.end,
                        },
                    }),
                    span: Span { start, end },
                }
            }
            token => {
                return Err(Error {
                    kind: ErrorKind::ExpectWqName,
                    span: token.span().clone(),
                });
            }
        };

        let matcher = match self.tokenizer.peek()? {
            Token::RBracket(..) => None,
            Token::Equal(token) => {
                let _ = self.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Equals,
                    span: token.span,
                })
            }
            Token::TildeEqual(token) => {
                let _ = self.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Tilde,
                    span: token.span,
                })
            }
            Token::BarEqual(token) => {
                let _ = self.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Bar,
                    span: token.span,
                })
            }
            Token::CaretEqual(token) => {
                let _ = self.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Caret,
                    span: token.span,
                })
            }
            Token::DollarEqual(token) => {
                let _ = self.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Dollar,
                    span: token.span,
                })
            }
            Token::AsteriskEqual(token) => {
                let _ = self.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Asterisk,
                    span: token.span,
                })
            }
            token => {
                return Err(Error {
                    kind: ErrorKind::ExpectAttributeSelectorMatcher,
                    span: token.span().clone(),
                });
            }
        };

        let value = match self.tokenizer.bump()? {
            Token::Ident(..) | Token::HashLBrace(..) => Some(AttributeSelectorValue::Ident(
                self.parse_interpolable_ident()?,
            )),
            Token::Str(str) => Some(AttributeSelectorValue::Str(str.into())),
            Token::RBracket(..) => None,
            token => {
                return Err(Error {
                    kind: ErrorKind::ExpectAttributeSelectorValue,
                    span: token.span().clone(),
                });
            }
        };

        let modifier = if value.is_some() {
            match self.tokenizer.peek()? {
                Token::Ident(..) | Token::HashLBrace(..) => {
                    let ident = self.parse_interpolable_ident()?;
                    let span = ident.span().clone();
                    Some(AttributeSelectorModifier { ident, span })
                }
                _ => None,
            }
        } else {
            None
        };

        let r_bracket = expect!(self, RBracket);
        Ok(AttributeSelector {
            name,
            matcher,
            value,
            modifier,
            span: Span {
                start: l_bracket.span.start,
                end: r_bracket.span.end,
            },
        })
    }

    fn parse_class_selector(&mut self) -> PResult<ClassSelector<'a>> {
        let dot = expect!(self, Dot);
        let ident = self.parse_interpolable_ident()?;
        let ident_span = ident.span();
        self.assert_no_ws_or_comment(&dot.span, ident_span)?;

        let span = Span {
            start: dot.span.start,
            end: ident_span.end,
        };
        Ok(ClassSelector { name: ident, span })
    }

    fn parse_complex_selector(&mut self) -> PResult<ComplexSelector<'a>> {
        let mut children = Vec::with_capacity(1);
        let first = self.parse_compound_selector()?;
        let mut span = first.span.clone();

        children.push(ComplexSelectorChild::CompoundSelector(first));
        while let Some(combinator) = self.parse_combinator()? {
            children.push(ComplexSelectorChild::Combinator(combinator));
            children.push(
                self.parse_compound_selector()
                    .map(ComplexSelectorChild::CompoundSelector)?,
            );
        }

        if let Some(last) = children.last() {
            span.end = last.span().end;
        }
        Ok(ComplexSelector { children, span })
    }

    fn parse_combinator(&mut self) -> PResult<Option<Combinator>> {
        let current_offset = self.tokenizer.current_offset();
        match self.tokenizer.peek()? {
            token @ Token::Ident(..)
            | token @ Token::Dot(..)
            | token @ Token::Hash(..)
            | token @ Token::Colon(..)
            | token @ Token::ColonColon(..)
            | token @ Token::Asterisk(..)
            | token @ Token::Ampersand(..)
            | token @ Token::Bar(..) // selector like `|type` (with <ns-prefix>)
                if current_offset < token.span().start =>
            {
                Ok(Some(Combinator {
                    kind: CombinatorKind::Descendant,
                    span: Span {
                        start: current_offset,
                        end: token.span().start,
                    },
                }))
            }
            Token::GreaterThan(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(Combinator {
                    kind: CombinatorKind::Child,
                    span: token.span,
                }))
            }
            Token::Plus(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(Combinator {
                    kind: CombinatorKind::NextSibling,
                    span: token.span,
                }))
            }
            Token::Tilde(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(Combinator {
                    kind: CombinatorKind::LaterSibling,
                    span: token.span,
                }))
            }
            Token::BarBar(token) => {
                let _ = self.tokenizer.bump();
                Ok(Some(Combinator {
                    kind: CombinatorKind::Column,
                    span: token.span,
                }))
            }
            _ => Ok(None),
        }
    }

    fn parse_compound_selector(&mut self) -> PResult<CompoundSelector<'a>> {
        let first = self.parse_simple_selector()?;
        let mut span = first.span().clone();

        let mut children = vec![first];
        loop {
            match self.tokenizer.peek()? {
                token @ Token::Dot(..)
                | token @ Token::Hash(..)
                | token @ Token::Colon(..)
                | token @ Token::ColonColon(..)
                | token @ Token::Ampersand(..)
                | token @ Token::Ident(..)
                | token @ Token::Asterisk(..)
                | token @ Token::HashLBrace(..)
                | token @ Token::NumberSign(..)
                | token @ Token::Bar(..)
                    if self.tokenizer.current_offset() == token.span().start =>
                {
                    children.push(self.parse_simple_selector()?)
                }
                _ => break,
            }
        }

        if let Some(last) = children.last() {
            span.end = last.span().end;
        }
        Ok(CompoundSelector { children, span })
    }

    fn parse_id_selector(&mut self) -> PResult<IdSelector<'a>> {
        match self.tokenizer.bump()? {
            Token::Hash(token) => {
                let first_span = Span {
                    start: token.span.start + 1,
                    end: token.span.end,
                };
                if token.value.starts_with(|c: char| c.is_ascii_digit()) {
                    return Err(Error {
                        kind: ErrorKind::InvalidIdSelectorName,
                        span: first_span,
                    });
                }
                let first = Ident {
                    name: token.value,
                    raw: token.raw_without_hash,
                    span: first_span,
                };
                let name = match self.tokenizer.peek()? {
                    Token::HashLBrace(token)
                        if matches!(self.syntax, Syntax::Scss)
                            && first.span.end == token.span.start =>
                    {
                        match self.parse_interpolable_ident()? {
                            InterpolableIdent::SassInterpolated(mut interpolation) => {
                                interpolation.elements.insert(
                                    0,
                                    SassInterpolatedIdentElement::Static(
                                        InterpolableIdentStaticPart {
                                            value: first.name,
                                            raw: first.raw,
                                            span: first.span,
                                        },
                                    ),
                                );
                                InterpolableIdent::SassInterpolated(interpolation)
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => InterpolableIdent::Literal(first),
                };
                let span = Span {
                    start: token.span.start,
                    end: name.span().end,
                };
                Ok(IdSelector { name, span })
            }
            Token::NumberSign(token) => {
                let name = self.parse_interpolable_ident()?;
                let span = Span {
                    start: token.span.start,
                    end: name.span().end,
                };
                Ok(IdSelector { name, span })
            }
            token => Err(Error {
                kind: ErrorKind::ExpectIdSelector,
                span: token.span().clone(),
            }),
        }
    }

    fn parse_nesting_selector(&mut self) -> PResult<NestingSelector> {
        let token = expect!(self, Ampersand);
        Ok(NestingSelector { span: token.span })
    }

    pub(super) fn parse_selector_list(&mut self) -> PResult<SelectorList<'a>> {
        let first = self.parse_complex_selector()?;
        let mut span = first.span.clone();

        let mut selectors = vec![first];
        while eat!(self, Comma).is_some() {
            selectors.push(self.parse_complex_selector()?);
        }

        if let Some(selector) = selectors.last() {
            span.end = selector.span.end;
        }
        Ok(SelectorList { selectors, span })
    }

    // https://www.w3.org/TR/selectors-4/#ref-for-typedef-simple-selector
    fn parse_simple_selector(&mut self) -> PResult<SimpleSelector<'a>> {
        match self.tokenizer.peek()? {
            Token::Dot(..) => self.parse_class_selector().map(SimpleSelector::Class),
            Token::Hash(..) | Token::NumberSign(..) => {
                self.parse_id_selector().map(SimpleSelector::Id)
            }
            Token::LBracket(..) => self
                .parse_attribute_selector()
                .map(SimpleSelector::Attribute),
            Token::Colon(..) => todo!(),
            Token::ColonColon(..) => todo!(),
            Token::Ident(..) | Token::Asterisk(..) | Token::HashLBrace(..) | Token::Bar(..) => {
                self.parse_type_selector().map(SimpleSelector::Type)
            }
            Token::Ampersand(..) => self.parse_nesting_selector().map(SimpleSelector::Nesting),
            token => Err(Error {
                kind: ErrorKind::ExpectSimpleSelector,
                span: token.span().clone(),
            }),
        }
    }

    fn parse_type_selector(&mut self) -> PResult<TypeSelector<'a>> {
        #[derive(Spanned)]
        enum IdentOrAsterisk<'a> {
            Ident(InterpolableIdent<'a>),
            Asterisk(token::Asterisk),
        }

        let ident_or_asterisk = match self.tokenizer.peek()? {
            Token::Ident(..) | Token::HashLBrace(..) => self
                .parse_interpolable_ident()
                .map(IdentOrAsterisk::Ident)
                .map(Some)?,
            Token::Asterisk(token) => {
                self.tokenizer.bump()?;
                Some(IdentOrAsterisk::Asterisk(token))
            }
            Token::Bar(..) => None,
            _ => unreachable!(),
        };

        match self.tokenizer.peek()? {
            Token::Bar(bar_token)
                if ident_or_asterisk
                    .as_ref()
                    .map(|t| t.span().end == bar_token.span.start)
                    .unwrap_or(true) =>
            {
                let token = self.tokenizer.bump()?;
                debug_assert!(matches!(&token, Token::Bar(..)));

                let prefix = match ident_or_asterisk {
                    Some(IdentOrAsterisk::Ident(ident)) => {
                        let mut span = ident.span().clone();
                        span.end = bar_token.span.end;
                        NsPrefix {
                            kind: Some(NsPrefixKind::Ident(ident)),
                            span,
                        }
                    }
                    Some(IdentOrAsterisk::Asterisk(asterisk)) => {
                        let mut span = asterisk.span.clone();
                        span.end = bar_token.span.end;
                        NsPrefix {
                            kind: Some(NsPrefixKind::Universal(NsPrefixUniversal {
                                span: asterisk.span,
                            })),
                            span,
                        }
                    }
                    None => NsPrefix {
                        kind: None,
                        span: bar_token.span,
                    },
                };

                match self.tokenizer.peek()? {
                    Token::Ident(..) | Token::HashLBrace(..) => {
                        let name = self.parse_interpolable_ident()?;
                        let name_span = name.span();
                        self.assert_no_ws_or_comment(&prefix.span, name_span)?;
                        let span = Span {
                            start: prefix.span.start,
                            end: name_span.end,
                        };
                        Ok(TypeSelector::TagName(TagNameSelector {
                            name: WqName {
                                name,
                                prefix: Some(prefix),
                                span: span.clone(),
                            },
                            span,
                        }))
                    }
                    Token::Asterisk(asterisk) => {
                        self.tokenizer.bump()?;
                        self.assert_no_ws_or_comment(&prefix.span, &asterisk.span)?;
                        let span = Span {
                            start: prefix.span.start,
                            end: asterisk.span.end,
                        };
                        Ok(TypeSelector::Universal(UniversalSelector {
                            prefix: Some(prefix),
                            span,
                        }))
                    }
                    token => Err(Error {
                        kind: ErrorKind::ExpectTypeSelector,
                        span: token.span().clone(),
                    }),
                }
            }

            _ => match ident_or_asterisk {
                Some(IdentOrAsterisk::Ident(ident)) => {
                    let span = ident.span().clone();
                    Ok(TypeSelector::TagName(TagNameSelector {
                        name: WqName {
                            name: ident,
                            prefix: None,
                            span: span.clone(),
                        },
                        span,
                    }))
                }
                Some(IdentOrAsterisk::Asterisk(asterisk)) => {
                    Ok(TypeSelector::Universal(UniversalSelector {
                        prefix: None,
                        span: asterisk.span,
                    }))
                }
                None => unreachable!(),
            },
        }
    }
}
