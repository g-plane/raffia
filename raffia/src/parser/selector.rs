use super::Parser;
use crate::{
    ast::*,
    eat,
    error::{Error, ErrorKind, PResult},
    expect, expect_without_ws_or_comments,
    pos::{Span, Spanned},
    tokenizer::{handle_escape, token, Token},
    util::{LastOfNonEmpty, PairedToken},
    Parse, Syntax,
};
use beef::Cow;
use raffia_derive::Spanned;
use smallvec::SmallVec;

// https://www.w3.org/TR/css-syntax-3/#the-anb-type
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for AnPlusB {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Dimension(token::Dimension {
                value,
                unit:
                    token::Ident {
                        name,
                        span: unit_span,
                        ..
                    },
                span,
            }) => {
                input.tokenizer.bump()?;
                if name.eq_ignore_ascii_case("n") {
                    match input.tokenizer.peek()? {
                        // syntax: <n-dimension> ['+' | '-'] <signless-integer>
                        // examples: '1n + 1', '1n - 1', '1n+ 1'
                        sign @ Token::Plus(..) | sign @ Token::Minus(..) => {
                            input.tokenizer.bump()?;
                            let number = expect_unsigned_int(input)?;
                            let span = Span {
                                start: span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: value.try_into()?,
                                b: if let Token::Plus(..) = sign { 1 } else { -1 }
                                    * i32::try_from(number)?,
                                span,
                            })
                        }

                        // syntax: <n-dimension> <signed-integer>
                        // examples: '1n +1', '1n -1'
                        Token::Number(number) => {
                            input.tokenizer.bump()?;
                            let span = Span {
                                start: span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: value.try_into()?,
                                b: number.try_into()?,
                                span,
                            })
                        }

                        // syntax: <n-dimension>
                        // examples: '1n'
                        _ => Ok(AnPlusB {
                            a: value.try_into()?,
                            b: 0,
                            span,
                        }),
                    }
                } else if name.eq_ignore_ascii_case("n-") {
                    // syntax: <ndash-dimension> <signless-integer>
                    // examples: '1n- 1'
                    let number = expect_unsigned_int(input)?;
                    let span = Span {
                        start: span.start,
                        end: number.span.end,
                    };
                    Ok(AnPlusB {
                        a: value.try_into()?,
                        b: -i32::try_from(number)?,
                        span,
                    })
                } else if let Some(digits) = name.strip_prefix("n-") {
                    // syntax: <ndashdigit-dimension>
                    // examples: '1n-1'
                    if digits.chars().any(|c| !c.is_ascii_digit()) {
                        return Err(Error {
                            kind: ErrorKind::ExpectUnsignedInteger,
                            span: Span {
                                start: unit_span.start + 2,
                                end: unit_span.end,
                            },
                        });
                    }
                    let b = digits.parse::<i32>().map_err(|_| Error {
                        kind: ErrorKind::ExpectInteger,
                        span: Span {
                            start: unit_span.start + 2,
                            end: unit_span.end,
                        },
                    })?;
                    Ok(AnPlusB {
                        a: value.try_into()?,
                        b: -b,
                        span,
                    })
                } else {
                    Err(Error {
                        kind: ErrorKind::InvalidAnPlusB,
                        span,
                    })
                }
            }

            Token::Plus(plus) => {
                input.tokenizer.bump()?;
                let ident = expect_without_ws_or_comments!(input, Ident);
                if ident.name.eq_ignore_ascii_case("n") {
                    match input.tokenizer.peek()? {
                        // syntax: +n ['+' | '-'] <signless-integer>
                        // examples: '+n + 1', '+n - 1', '+n+ 1'
                        sign @ Token::Plus(..) | sign @ Token::Minus(..) => {
                            input.tokenizer.bump()?;
                            let number = expect_unsigned_int(input)?;
                            let span = Span {
                                start: plus.span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: 1,
                                b: if let Token::Plus(..) = sign { 1 } else { -1 }
                                    * i32::try_from(number)?,
                                span,
                            })
                        }

                        // syntax: +n <signed-integer>
                        // examples: '+n +1', '+n -1'
                        Token::Number(number) => {
                            input.tokenizer.bump()?;
                            let span = Span {
                                start: plus.span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: 1,
                                b: number.try_into()?,
                                span,
                            })
                        }

                        // syntax: +n
                        _ => Ok(AnPlusB {
                            a: 1,
                            b: 0,
                            span: Span {
                                start: plus.span.start,
                                end: ident.span.end,
                            },
                        }),
                    }
                } else if ident.name.eq_ignore_ascii_case("n-") {
                    // syntax: +n- <signless-integer>
                    // examples: '+n- 1'
                    let number = expect_unsigned_int(input)?;
                    let span = Span {
                        start: plus.span.start,
                        end: number.span.end,
                    };
                    Ok(AnPlusB {
                        a: 1,
                        b: -i32::try_from(number)?,
                        span,
                    })
                } else if let Some(digits) = ident.name.strip_prefix("n-") {
                    // syntax: +<ndashdigit-ident>
                    // examples: '+n-1'
                    if digits.chars().any(|c| !c.is_ascii_digit()) {
                        return Err(Error {
                            kind: ErrorKind::ExpectUnsignedInteger,
                            span: Span {
                                start: ident.span.start + 2,
                                end: ident.span.end,
                            },
                        });
                    }
                    let b = digits.parse::<i32>().map_err(|_| Error {
                        kind: ErrorKind::ExpectInteger,
                        span: Span {
                            start: ident.span.start + 2,
                            end: ident.span.end,
                        },
                    })?;
                    Ok(AnPlusB {
                        a: 1,
                        b: -b,
                        span: Span {
                            start: plus.span.start,
                            end: ident.span.end,
                        },
                    })
                } else {
                    Err(Error {
                        kind: ErrorKind::InvalidAnPlusB,
                        span: Span {
                            start: plus.span.start,
                            end: ident.span.end,
                        },
                    })
                }
            }

            Token::Ident(ident) => {
                input.tokenizer.bump()?;
                if ident.name.eq_ignore_ascii_case("n") {
                    match input.tokenizer.peek()? {
                        // syntax: n ['+' | '-'] <signless-integer>
                        // examples: 'n + 1', 'n - 1', 'n+ 1'
                        sign @ Token::Plus(..) | sign @ Token::Minus(..) => {
                            input.tokenizer.bump()?;
                            let number = expect_unsigned_int(input)?;
                            let span = Span {
                                start: ident.span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: 1,
                                b: if let Token::Plus(..) = sign { 1 } else { -1 }
                                    * i32::try_from(number)?,
                                span,
                            })
                        }

                        // syntax: n <signed-integer>
                        // examples: 'n +1', 'n -1'
                        Token::Number(number) => {
                            input.tokenizer.bump()?;
                            let span = Span {
                                start: ident.span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: 1,
                                b: number.try_into()?,
                                span,
                            })
                        }

                        // syntax: n
                        _ => Ok(AnPlusB {
                            a: 1,
                            b: 0,
                            span: ident.span,
                        }),
                    }
                } else if ident.name.eq_ignore_ascii_case("n-") {
                    // syntax: n- <signless-integer>
                    // examples: 'n- 1'
                    let number = expect_unsigned_int(input)?;
                    let span = Span {
                        start: ident.span.start,
                        end: number.span.end,
                    };
                    Ok(AnPlusB {
                        a: 1,
                        b: -i32::try_from(number)?,
                        span,
                    })
                } else if let Some(digits) = ident.name.strip_prefix("n-") {
                    // syntax: <ndashdigit-ident>
                    // examples: 'n-1'
                    if digits.chars().any(|c| !c.is_ascii_digit()) {
                        return Err(Error {
                            kind: ErrorKind::ExpectUnsignedInteger,
                            span: Span {
                                start: ident.span.start + 2,
                                end: ident.span.end,
                            },
                        });
                    }
                    let b = digits.parse::<i32>().map_err(|_| Error {
                        kind: ErrorKind::ExpectInteger,
                        span: Span {
                            start: ident.span.start + 2,
                            end: ident.span.end,
                        },
                    })?;
                    Ok(AnPlusB {
                        a: 1,
                        b: -b,
                        span: ident.span,
                    })
                } else if ident.name.eq_ignore_ascii_case("-n") {
                    match input.tokenizer.peek()? {
                        // syntax: -n ['+' | '-'] <signless-integer>
                        // examples: '-n + 1', '-n - 1', '-n+ 1'
                        sign @ Token::Plus(..) | sign @ Token::Minus(..) => {
                            input.tokenizer.bump()?;
                            let number = expect_unsigned_int(input)?;
                            let span = Span {
                                start: ident.span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: -1,
                                b: if let Token::Plus(..) = sign { 1 } else { -1 }
                                    * i32::try_from(number)?,
                                span,
                            })
                        }

                        // syntax: -n <signed-integer>
                        // examples: '-n +1', '-n -1'
                        Token::Number(number) => {
                            input.tokenizer.bump()?;
                            let span = Span {
                                start: ident.span.start,
                                end: number.span.end,
                            };
                            Ok(AnPlusB {
                                a: -1,
                                b: number.try_into()?,
                                span,
                            })
                        }

                        // syntax: -n
                        _ => Ok(AnPlusB {
                            a: -1,
                            b: 0,
                            span: ident.span,
                        }),
                    }
                } else if ident.name.eq_ignore_ascii_case("-n-") {
                    // syntax: -n- <signless-integer>
                    // examples: '-n- 1'
                    let number = expect_unsigned_int(input)?;
                    let span = Span {
                        start: ident.span.start,
                        end: number.span.end,
                    };
                    Ok(AnPlusB {
                        a: -1,
                        b: -i32::try_from(number)?,
                        span,
                    })
                } else if let Some(digits) = ident.name.strip_prefix("-n-") {
                    // syntax: -n-<ndashdigit-ident>
                    // examples: '-n-1'
                    if digits.chars().any(|c| !c.is_ascii_digit()) {
                        return Err(Error {
                            kind: ErrorKind::ExpectUnsignedInteger,
                            span: Span {
                                start: ident.span.start + 3,
                                end: ident.span.end,
                            },
                        });
                    }
                    let b = digits.parse::<i32>().map_err(|_| Error {
                        kind: ErrorKind::ExpectInteger,
                        span: Span {
                            start: ident.span.start + 3,
                            end: ident.span.end,
                        },
                    })?;
                    Ok(AnPlusB {
                        a: -1,
                        b: -b,
                        span: ident.span,
                    })
                } else {
                    Err(Error {
                        kind: ErrorKind::InvalidAnPlusB,
                        span: ident.span,
                    })
                }
            }

            token => Err(Error {
                kind: ErrorKind::InvalidAnPlusB,
                span: token.span().clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for AttributeSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let l_bracket = expect!(input, LBracket);

        let name = match input.tokenizer.peek()? {
            Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                let ident = input.parse::<InterpolableIdent>()?;
                let ident_span = ident.span();
                if let Some(bar_token) = eat!(input, Bar) {
                    input.assert_no_ws_or_comment(ident_span, &bar_token.span)?;

                    let name = input.parse::<InterpolableIdent>()?;
                    let name_span = name.span();
                    input.assert_no_ws_or_comment(&bar_token.span, name_span)?;

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
                let bar_token = expect!(input, Bar);
                let name = input.parse::<InterpolableIdent>()?;

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
                let name = input.parse::<InterpolableIdent>()?;

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

        let matcher = match input.tokenizer.peek()? {
            Token::RBracket(..) => None,
            Token::Equal(token) => {
                let _ = input.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Equals,
                    span: token.span,
                })
            }
            Token::TildeEqual(token) => {
                let _ = input.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Tilde,
                    span: token.span,
                })
            }
            Token::BarEqual(token) => {
                let _ = input.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Bar,
                    span: token.span,
                })
            }
            Token::CaretEqual(token) => {
                let _ = input.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Caret,
                    span: token.span,
                })
            }
            Token::DollarEqual(token) => {
                let _ = input.tokenizer.bump();
                Some(AttributeSelectorMatcher {
                    kind: AttributeSelectorMatcherKind::Dollar,
                    span: token.span,
                })
            }
            Token::AsteriskEqual(token) => {
                let _ = input.tokenizer.bump();
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

        let value = if matcher.is_some() {
            match input.tokenizer.peek()? {
                Token::Ident(..) | Token::HashLBrace(..) => {
                    Some(AttributeSelectorValue::Ident(input.parse()?))
                }
                Token::Str(..) | Token::StrTemplate(..) => {
                    Some(AttributeSelectorValue::Str(input.parse()?))
                }
                Token::RBracket(..) => None,
                token => {
                    return Err(Error {
                        kind: ErrorKind::ExpectAttributeSelectorValue,
                        span: token.span().clone(),
                    });
                }
            }
        } else {
            None
        };

        let modifier = if value.is_some() {
            match input.tokenizer.peek()? {
                Token::Ident(..) | Token::HashLBrace(..) => {
                    let ident = input.parse::<InterpolableIdent>()?;
                    let span = ident.span().clone();
                    Some(AttributeSelectorModifier { ident, span })
                }
                _ => None,
            }
        } else {
            None
        };

        let r_bracket = expect!(input, RBracket);
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
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ClassSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let dot = expect!(input, Dot);
        let start = dot.span.start;
        let end;
        let name = if input.syntax == Syntax::Css {
            let ident = expect_without_ws_or_comments!(input, Ident);
            end = ident.span.end;
            InterpolableIdent::Literal(ident.into())
        } else {
            let ident = input.parse::<InterpolableIdent>()?;
            let ident_span = ident.span();
            input.assert_no_ws_or_comment(&dot.span, ident_span)?;
            end = ident_span.end;
            ident
        };

        Ok(ClassSelector {
            name,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComplexSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let mut children = SmallVec::with_capacity(3);
        let first = input.parse::<CompoundSelector>()?;
        let mut span = first.span.clone();

        children.push(ComplexSelectorChild::CompoundSelector(first));
        while let Some(combinator) = input.parse_combinator()? {
            children.push(ComplexSelectorChild::Combinator(combinator));
            children.push(input.parse().map(ComplexSelectorChild::CompoundSelector)?);
        }

        span.end = children.last_of_non_empty().span().end;
        Ok(ComplexSelector { children, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CompoundSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<SimpleSelector>()?;
        let mut span = first.span().clone();

        let mut children = Vec::with_capacity(2);
        children.push(first);
        while !input.tokenizer.has_ws_or_comments() {
            if let Ok(child) = input.try_parse(|parser| parser.parse()) {
                children.push(child);
            } else {
                break;
            }
        }

        span.end = children.last_of_non_empty().span().end;
        Ok(CompoundSelector { children, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for CompoundSelectorList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<CompoundSelector>()?;
        let mut span = first.span.clone();

        let mut selectors = vec![first];
        while eat!(input, Comma).is_some() {
            selectors.push(input.parse()?);
        }

        span.end = selectors.last_of_non_empty().span.end;
        Ok(CompoundSelectorList { selectors, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for IdSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.bump()? {
            Token::Hash(token) => {
                let first_span = Span {
                    start: token.span.start + 1,
                    end: token.span.end,
                };
                let raw = token.raw_without_hash;
                if raw.starts_with(|c: char| c.is_ascii_digit()) {
                    input.recoverable_errors.push(Error {
                        kind: ErrorKind::InvalidIdSelectorName,
                        span: token.span.clone(),
                    });
                }
                let value = if token.escaped {
                    handle_escape(raw).map_err(|kind| Error {
                        kind,
                        span: token.span.clone(),
                    })?
                } else {
                    Cow::from(raw)
                };
                let first = Ident {
                    name: value,
                    raw: token.raw_without_hash,
                    span: first_span,
                };
                let name = match input.tokenizer.peek()? {
                    Token::HashLBrace(token)
                        if matches!(input.syntax, Syntax::Scss | Syntax::Sass)
                            && first.span.end == token.span.start =>
                    {
                        match input.parse()? {
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
                let name = input.parse::<InterpolableIdent>()?;
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
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LanguageRange<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Str(..) | Token::StrTemplate(..) => input.parse().map(LanguageRange::Str),
            _ => input.parse().map(LanguageRange::Ident),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for LanguageRangeList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<LanguageRange>()?;
        let mut span = first.span().clone();

        let mut ranges = vec![first];
        while eat!(input, Comma).is_some() {
            ranges.push(input.parse()?);
        }

        span.end = ranges.last_of_non_empty().span().end;
        Ok(LanguageRangeList { ranges, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for NestingSelector {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let token = expect!(input, Ampersand);
        Ok(NestingSelector { span: token.span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Nth<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("odd") => {
                Ok(Nth::Odd(input.parse()?))
            }
            Token::Ident(ident) if ident.name.eq_ignore_ascii_case("even") => {
                Ok(Nth::Even(input.parse()?))
            }
            Token::Number(..) => {
                let number: Number = expect!(input, Number).try_into()?;
                if number.value.fract() == 0.0 {
                    Ok(Nth::Integer(number))
                } else {
                    Err(Error {
                        kind: ErrorKind::ExpectInteger,
                        span: number.span,
                    })
                }
            }
            _ => Ok(Nth::AnPlusB(input.parse()?)),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for PseudoClassSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let colon = expect!(input, Colon);
        let mut end;
        let name = if input.syntax == Syntax::Css {
            let ident = expect_without_ws_or_comments!(input, Ident);
            end = ident.span.end;
            InterpolableIdent::Literal(ident.into())
        } else {
            let name = input.parse::<InterpolableIdent>()?;
            let name_span = name.span();
            end = name_span.end;
            input.assert_no_ws_or_comment(&colon.span, name_span)?;
            name
        };

        let arg = match input.tokenizer.peek()? {
            Token::LParen(l_paren) if l_paren.span.start == end => {
                expect!(input, LParen);
                let arg = match &name {
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("nth-child")
                            || name.eq_ignore_ascii_case("nth-last-child")
                            || name.eq_ignore_ascii_case("nth-of-type")
                            || name.eq_ignore_ascii_case("nth-last-of-type")
                            || name.eq_ignore_ascii_case("nth-col")
                            || name.eq_ignore_ascii_case("nth-last-col") =>
                    {
                        input.parse().map(PseudoClassSelectorArg::Nth)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("not")
                            || name.eq_ignore_ascii_case("is")
                            || name.eq_ignore_ascii_case("where")
                            || name.eq_ignore_ascii_case("matches") =>
                    {
                        input.parse().map(PseudoClassSelectorArg::SelectorList)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("has") =>
                    {
                        input
                            .parse()
                            .map(PseudoClassSelectorArg::RelativeSelectorList)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("dir") =>
                    {
                        input.parse().map(PseudoClassSelectorArg::Ident)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("lang") =>
                    {
                        input
                            .parse()
                            .map(PseudoClassSelectorArg::LanguageRangeList)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("-moz-any")
                            || name.eq_ignore_ascii_case("-webkit-any")
                            || name.eq_ignore_ascii_case("current")
                            || name.eq_ignore_ascii_case("past")
                            || name.eq_ignore_ascii_case("future") =>
                    {
                        input
                            .parse()
                            .map(PseudoClassSelectorArg::CompoundSelectorList)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("host")
                            || name.eq_ignore_ascii_case("host-context") =>
                    {
                        input
                            .parse()
                            .map(PseudoClassSelectorArg::CompoundSelector)?
                    }
                    _ => input
                        .parse_pseudo_arg_tokens(l_paren.span.end)
                        .map(PseudoClassSelectorArg::TokenSeq)?,
                };

                end = expect!(input, RParen).span.end;
                Some(arg)
            }
            _ => None,
        };

        let span = Span {
            start: colon.span.start,
            end,
        };
        Ok(PseudoClassSelector { name, arg, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for PseudoElementSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let colon_colon = expect!(input, ColonColon);
        let mut end;
        let name = if input.syntax == Syntax::Css {
            let ident = expect_without_ws_or_comments!(input, Ident);
            end = ident.span.end;
            InterpolableIdent::Literal(ident.into())
        } else {
            let name = input.parse::<InterpolableIdent>()?;
            let name_span = name.span();
            end = name_span.end;
            input.assert_no_ws_or_comment(&colon_colon.span, name_span)?;
            name
        };

        let arg = match input.tokenizer.peek()? {
            Token::LParen(l_paren) if l_paren.span.start == end => {
                expect!(input, LParen);
                let arg = match &name {
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("part") =>
                    {
                        input.parse().map(PseudoElementSelectorArg::Ident)?
                    }
                    InterpolableIdent::Literal(Ident { name, .. })
                        if name.eq_ignore_ascii_case("cue")
                            || name.eq_ignore_ascii_case("cue-region")
                            || name.eq_ignore_ascii_case("slotted") =>
                    {
                        input
                            .parse()
                            .map(PseudoElementSelectorArg::CompoundSelector)?
                    }
                    _ => input
                        .parse_pseudo_arg_tokens(l_paren.span.end)
                        .map(PseudoElementSelectorArg::TokenSeq)?,
                };

                end = expect!(input, RParen).span.end;
                Some(arg)
            }
            _ => None,
        };

        let span = Span {
            start: colon_colon.span.start,
            end,
        };
        Ok(PseudoElementSelector { name, arg, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for RelativeSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let combinator = input.parse_combinator()?;
        let complex_selector = input.parse::<ComplexSelector>()?;
        let mut span = complex_selector.span.clone();
        if let Some(combinator) = &combinator {
            span.start = combinator.span.start;
        }
        Ok(RelativeSelector {
            combinator,
            complex_selector,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for RelativeSelectorList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<RelativeSelector>()?;
        let mut span = first.span.clone();

        let mut selectors = vec![first];
        while eat!(input, Comma).is_some() {
            selectors.push(input.parse()?);
        }

        span.end = selectors.last_of_non_empty().span.end;
        Ok(RelativeSelectorList { selectors, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SelectorList<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<ComplexSelector>()?;
        let mut span = first.span.clone();

        let mut selectors = SmallVec::with_capacity(2);
        selectors.push(first);
        while eat!(input, Comma).is_some() {
            selectors.push(input.parse()?);
        }

        span.end = selectors.last_of_non_empty().span.end;
        Ok(SelectorList { selectors, span })
    }
}

// https://www.w3.org/TR/selectors-4/#ref-for-typedef-simple-selector
impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SimpleSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.peek()? {
            Token::Dot(..) => input.parse().map(SimpleSelector::Class),
            Token::Hash(..) | Token::NumberSign(..) => input.parse().map(SimpleSelector::Id),
            Token::LBracket(..) => input.parse().map(SimpleSelector::Attribute),
            Token::Colon(..) => input.parse().map(SimpleSelector::PseudoClass),
            Token::ColonColon(..) => input.parse().map(SimpleSelector::PseudoElement),
            Token::Ident(..) | Token::Asterisk(..) | Token::HashLBrace(..) | Token::Bar(..) => {
                input.parse().map(SimpleSelector::Type)
            }
            Token::Ampersand(..) => input.parse().map(SimpleSelector::Nesting),
            Token::Percent(..) if matches!(input.syntax, Syntax::Scss | Syntax::Sass) => {
                input.parse().map(SimpleSelector::SassPlaceholder)
            }
            token => Err(Error {
                kind: ErrorKind::ExpectSimpleSelector,
                span: token.span().clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for TypeSelector<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        #[derive(Spanned)]
        enum IdentOrAsterisk<'s> {
            Ident(InterpolableIdent<'s>),
            Asterisk(token::Asterisk),
        }

        let ident_or_asterisk = match input.tokenizer.peek()? {
            Token::Ident(..) | Token::HashLBrace(..) => {
                input.parse().map(IdentOrAsterisk::Ident).map(Some)?
            }
            Token::Asterisk(token) => {
                input.tokenizer.bump()?;
                Some(IdentOrAsterisk::Asterisk(token))
            }
            Token::Bar(..) => None,
            _ => unreachable!(),
        };

        match input.tokenizer.peek()? {
            Token::Bar(bar_token)
                if ident_or_asterisk
                    .as_ref()
                    .map(|t| t.span().end == bar_token.span.start)
                    .unwrap_or(true) =>
            {
                let token = input.tokenizer.bump()?;
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

                match input.tokenizer.peek()? {
                    Token::Ident(..) | Token::HashLBrace(..) => {
                        let name = input.parse::<InterpolableIdent>()?;
                        let name_span = name.span();
                        input.assert_no_ws_or_comment(&prefix.span, name_span)?;
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
                        input.tokenizer.bump()?;
                        input.assert_no_ws_or_comment(&prefix.span, &asterisk.span)?;
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

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn parse_combinator(&mut self) -> PResult<Option<Combinator>> {
        let current_offset = self.tokenizer.current_offset();
        match self.tokenizer.peek()? {
            Token::Ident(token::Ident { span, .. })
            | Token::Dot(token::Dot { span })
            | Token::Hash(token::Hash { span, .. })
            | Token::Colon(token::Colon { span, .. })
            | Token::ColonColon(token::ColonColon { span })
            | Token::LBracket(token::LBracket { span, .. })
            | Token::Asterisk(token::Asterisk { span })
            | Token::Ampersand(token::Ampersand { span })
            | Token::Bar(token::Bar { span }) // selector like `|type` (with <ns-prefix>)
                if current_offset < span.start =>
            {
                Ok(Some(Combinator {
                    kind: CombinatorKind::Descendant,
                    span: Span {
                        start: current_offset,
                        end: span.start,
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

    fn parse_pseudo_arg_tokens(&mut self, start: usize) -> PResult<TokenSeq<'s>> {
        let mut tokens = Vec::with_capacity(1);
        let mut pairs = Vec::with_capacity(1);
        loop {
            match self.tokenizer.peek()? {
                Token::LParen(..) | Token::UrlPrefix(..) => {
                    pairs.push(PairedToken::Paren);
                }
                Token::RParen(..) => {
                    if let Some(PairedToken::Paren) = pairs.pop() {
                    } else {
                        break;
                    }
                }
                Token::LBracket(..) => {
                    pairs.push(PairedToken::Bracket);
                }
                Token::RBracket(..) => {
                    if let Some(PairedToken::Bracket) = pairs.pop() {
                    } else {
                        break;
                    }
                }
                Token::LBrace(..) => {
                    pairs.push(PairedToken::Brace);
                }
                Token::RBrace(..) => {
                    if let Some(PairedToken::Brace) = pairs.pop() {
                    } else {
                        break;
                    }
                }
                _ => {}
            }
            tokens.push(self.tokenizer.bump()?);
        }
        let span = Span {
            start: tokens
                .first()
                .map(|token| token.span().start)
                .unwrap_or(start),
            end: tokens
                .last()
                .map(|token| token.span().end)
                .unwrap_or_else(|| self.tokenizer.current_offset()),
        };
        Ok(TokenSeq { tokens, span })
    }
}

fn expect_unsigned_int<'cmt, 's: 'cmt>(input: &mut Parser<'cmt, 's>) -> PResult<token::Number<'s>> {
    let number = expect!(input, Number);
    if number.raw.chars().any(|c| !c.is_ascii_digit()) {
        Err(Error {
            kind: ErrorKind::ExpectUnsignedInteger,
            span: number.span,
        })
    } else {
        Ok(number)
    }
}
