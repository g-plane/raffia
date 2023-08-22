use super::{state::QualifiedRuleContext, Parser};
use crate::{
    ast::*,
    bump, eat,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util::{assert_no_ws_or_comment, handle_escape, CowStr},
    Parse, Syntax,
};

const PRECEDENCE_MULTIPLY: u8 = 2;
const PRECEDENCE_PLUS: u8 = 1;

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub(in crate::parser) fn parse_calc_expr(&mut self) -> PResult<ComponentValue<'s>> {
        self.parse_calc_expr_recursively(0)
    }

    fn parse_calc_expr_recursively(&mut self, precedence: u8) -> PResult<ComponentValue<'s>> {
        let mut left = if precedence >= PRECEDENCE_MULTIPLY {
            if eat!(self, LParen).is_some() {
                let expr = self.parse_calc_expr()?;
                expect!(self, RParen);
                expr
            } else {
                self.parse_component_value_atom()?
            }
        } else {
            self.parse_calc_expr_recursively(precedence + 1)?
        };

        loop {
            let operator = match &peek!(self).token {
                Token::Asterisk(..) if precedence == PRECEDENCE_MULTIPLY => CalcOperator {
                    kind: CalcOperatorKind::Multiply,
                    span: bump!(self).span,
                },
                Token::Solidus(..) if precedence == PRECEDENCE_MULTIPLY => CalcOperator {
                    kind: CalcOperatorKind::Division,
                    span: bump!(self).span,
                },
                Token::Plus(..) if precedence == PRECEDENCE_PLUS => CalcOperator {
                    kind: CalcOperatorKind::Plus,
                    span: bump!(self).span,
                },
                Token::Minus(..) if precedence == PRECEDENCE_PLUS => CalcOperator {
                    kind: CalcOperatorKind::Minus,
                    span: bump!(self).span,
                },
                _ => break,
            };

            let right = self.parse_calc_expr_recursively(precedence + 1)?;
            let span = Span {
                start: left.span().start,
                end: right.span().end,
            };
            left = ComponentValue::Calc(Calc {
                left: Box::new(left),
                op: operator,
                right: Box::new(right),
                span,
            });
        }

        Ok(left)
    }

    pub(super) fn parse_component_value_atom(&mut self) -> PResult<ComponentValue<'s>> {
        let token_with_span = peek!(self);
        match &token_with_span.token {
            Token::Ident(token) => {
                if token.name().eq_ignore_ascii_case("url") {
                    match self.try_parse(Url::parse) {
                        Ok(url) => return Ok(ComponentValue::Url(url)),
                        Err(Error {
                            kind: ErrorKind::TryParseError,
                            ..
                        }) => {}
                        Err(error) => {
                            return if matches!(self.syntax, Syntax::Scss | Syntax::Sass) {
                                let function_name = expect!(self, Ident).into();
                                self.parse_function(InterpolableIdent::Literal(function_name))
                                    .map(ComponentValue::Function)
                                    .map_err(|_| error)
                            } else {
                                Err(error)
                            };
                        }
                    }
                }
                let ident = self.parse::<InterpolableIdent>()?;
                let ident_end = ident.span().end;
                match peek!(self) {
                    TokenWithSpan {
                        token: Token::LParen(..),
                        span,
                    } if span.start == ident_end => {
                        return match ident {
                            InterpolableIdent::Literal(ident)
                                if ident.name.eq_ignore_ascii_case("src") =>
                            {
                                self.parse_src_url(ident).map(ComponentValue::Url)
                            }
                            ident => self.parse_function(ident).map(ComponentValue::Function),
                        };
                    }
                    TokenWithSpan {
                        token: Token::Dot(..),
                        span,
                    } if matches!(self.syntax, Syntax::Scss | Syntax::Sass)
                        && span.start == ident_end =>
                    {
                        if let InterpolableIdent::Literal(module) = ident {
                            let name = self.parse_sass_qualified_name(module)?;
                            return if let SassQualifiedName {
                                member: SassModuleMemberName::Ident(..),
                                ..
                            } = name
                            {
                                let (_, lparen_span) = expect!(self, LParen);
                                assert_no_ws_or_comment(&name.span, &lparen_span)?;
                                let args = self.parse_function_args()?;
                                let (_, Span { end, .. }) = expect!(self, RParen);
                                let span = Span {
                                    start: name.span.start,
                                    end,
                                };
                                Ok(ComponentValue::Function(Function {
                                    name: FunctionName::SassQualifiedName(name),
                                    args,
                                    span,
                                }))
                            } else {
                                Ok(ComponentValue::SassQualifiedName(name))
                            };
                        }
                    }
                    _ => {}
                }
                match ident {
                    InterpolableIdent::Literal(ident) if ident.raw.eq_ignore_ascii_case("u") => {
                        match peek!(self) {
                            TokenWithSpan {
                                token: Token::Plus(..),
                                span,
                            } if span.start == ident_end => self
                                .parse_unicode_range(ident)
                                .map(ComponentValue::UnicodeRange),
                            TokenWithSpan {
                                token: Token::Number(token),
                                span,
                            } if token.raw.starts_with('+') && span.start == ident_end => self
                                .parse_unicode_range(ident)
                                .map(ComponentValue::UnicodeRange),
                            TokenWithSpan {
                                token: Token::Dimension(token),
                                span,
                            } if token.value.raw.starts_with('+') && span.start == ident_end => {
                                self.parse_unicode_range(ident)
                                    .map(ComponentValue::UnicodeRange)
                            }
                            _ => Ok(ComponentValue::InterpolableIdent(
                                InterpolableIdent::Literal(ident),
                            )),
                        }
                    }
                    _ => Ok(ComponentValue::InterpolableIdent(ident)),
                }
            }
            Token::Solidus(..) | Token::Comma(..) => self.parse().map(ComponentValue::Delimiter),
            Token::Number(..) => self.parse().map(ComponentValue::Number),
            Token::Dimension(..) => self.parse().map(ComponentValue::Dimension),
            Token::Percentage(..) => self.parse().map(ComponentValue::Percentage),
            Token::Hash(..) => {
                if self.syntax == Syntax::Less {
                    self.try_parse(Parser::parse_less_maybe_mixin_call_or_with_lookups)
                        .or_else(|_| self.parse().map(ComponentValue::HexColor))
                } else {
                    self.parse().map(ComponentValue::HexColor)
                }
            }
            Token::Str(..) => self
                .parse()
                .map(InterpolableStr::Literal)
                .map(ComponentValue::InterpolableStr),
            Token::LBracket(..) => self.parse().map(ComponentValue::BracketBlock),
            Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.parse().map(ComponentValue::SassVariable)
            }
            Token::LParen(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                match self.try_parse(SassParenthesizedExpression::parse) {
                    Ok(expr) => Ok(ComponentValue::SassParenthesizedExpression(expr)),
                    Err(err) => self.parse().map(ComponentValue::SassMap).map_err(|_| err),
                }
            }
            Token::HashLBrace(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                let ident = self.parse_sass_interpolated_ident()?;
                match peek!(self) {
                    TokenWithSpan {
                        token: Token::LParen(..),
                        span,
                    } if span.start == ident.span().end => {
                        self.parse_function(ident).map(ComponentValue::Function)
                    }
                    _ => Ok(ComponentValue::InterpolableIdent(ident)),
                }
            }
            Token::StrTemplate(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => self
                .parse()
                .map(InterpolableStr::SassInterpolated)
                .map(ComponentValue::InterpolableStr),
            Token::Ampersand(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                self.parse().map(ComponentValue::SassParentSelector)
            }
            Token::LBrace(..)
                if self.syntax == Syntax::Scss
                    && matches!(
                        self.state.qualified_rule_ctx,
                        Some(QualifiedRuleContext::DeclarationValue)
                    ) =>
            {
                self.parse().map(ComponentValue::SassNestingDeclaration)
            }
            Token::Indent(..)
                if self.syntax == Syntax::Sass
                    && matches!(
                        self.state.qualified_rule_ctx,
                        Some(QualifiedRuleContext::DeclarationValue)
                    ) =>
            {
                self.parse().map(ComponentValue::SassNestingDeclaration)
            }
            Token::AtKeyword(..) if self.syntax == Syntax::Less => {
                self.parse_less_maybe_variable_or_with_lookups()
            }
            Token::Dot(..) if self.syntax == Syntax::Less => {
                self.parse_less_maybe_mixin_call_or_with_lookups()
            }
            Token::StrTemplate(..) if self.syntax == Syntax::Less => self
                .parse()
                .map(InterpolableStr::LessInterpolated)
                .map(ComponentValue::InterpolableStr),
            Token::At(..) if self.syntax == Syntax::Less => {
                self.parse().map(ComponentValue::LessVariableVariable)
            }
            Token::DollarVar(..) if self.syntax == Syntax::Less => {
                self.parse().map(ComponentValue::LessPropertyVariable)
            }
            Token::Tilde(..) if self.syntax == Syntax::Less => {
                if let Ok(list_function_call) = self.try_parse(LessListFunctionCall::parse) {
                    Ok(ComponentValue::LessListFunctionCall(list_function_call))
                } else if let Ok(less_escaped_str) = self.try_parse(LessEscapedStr::parse) {
                    Ok(ComponentValue::LessEscapedStr(less_escaped_str))
                } else {
                    self.parse().map(ComponentValue::LessJavaScriptSnippet)
                }
            }
            Token::Percent(..) if self.syntax == Syntax::Less => self
                .try_parse(LessFormatFunctionCall::parse)
                .map(ComponentValue::LessFormatFunctionCall)
                .or_else(|_| self.parse().map(ComponentValue::LessPercentKeyword)),
            Token::BacktickCode(..) if self.syntax == Syntax::Less => {
                self.parse().map(ComponentValue::LessJavaScriptSnippet)
            }
            _ => Err(Error {
                kind: ErrorKind::ExpectComponentValue,
                span: token_with_span.span.clone(),
            }),
        }
    }

    pub(in crate::parser) fn parse_component_values(
        &mut self,
        allow_comma: bool,
    ) -> PResult<ComponentValues<'s>> {
        let first = self.parse::<ComponentValue>()?;
        let mut span = first.span().clone();

        let mut values = Vec::with_capacity(4);
        values.push(first);
        loop {
            match &peek!(self).token {
                Token::RBrace(..)
                | Token::RParen(..)
                | Token::Semicolon(..)
                | Token::Dedent(..)
                | Token::Linebreak(..)
                | Token::Exclamation(..)
                | Token::Eof(..) => break,
                Token::Comma(..) => {
                    if allow_comma {
                        values.push(self.parse().map(ComponentValue::Delimiter)?);
                    } else {
                        break;
                    }
                }
                _ => values.push(self.parse()?),
            }
        }

        // SAFETY: it has at least one element.
        span.end = unsafe {
            let index = values.len() - 1;
            values.get_unchecked(index).span().end
        };
        Ok(ComponentValues { values, span })
    }

    pub(super) fn parse_dashed_ident(&mut self) -> PResult<InterpolableIdent<'s>> {
        let ident = self.parse()?;
        match &ident {
            InterpolableIdent::Literal(ident) if !ident.name.starts_with("--") => {
                self.recoverable_errors.push(Error {
                    kind: ErrorKind::ExpectDashedIdent,
                    span: ident.span.clone(),
                });
            }
            _ => {}
        }
        Ok(ident)
    }

    pub(super) fn parse_function(&mut self, name: InterpolableIdent<'s>) -> PResult<Function<'s>> {
        expect!(self, LParen);
        let args = if let Token::RParen(..) = &peek!(self).token {
            vec![]
        } else {
            match &name {
                InterpolableIdent::Literal(ident)
                    if ident.name.eq_ignore_ascii_case("calc")
                        || ident.name.eq_ignore_ascii_case("-webkit-calc")
                        || ident.name.eq_ignore_ascii_case("-moz-calc")
                        || ident.name.eq_ignore_ascii_case("min")
                        || ident.name.eq_ignore_ascii_case("max")
                        || ident.name.eq_ignore_ascii_case("clamp")
                        || ident.name.eq_ignore_ascii_case("sin")
                        || ident.name.eq_ignore_ascii_case("cos")
                        || ident.name.eq_ignore_ascii_case("tan")
                        || ident.name.eq_ignore_ascii_case("asin")
                        || ident.name.eq_ignore_ascii_case("acos")
                        || ident.name.eq_ignore_ascii_case("atan")
                        || ident.name.eq_ignore_ascii_case("sqrt")
                        || ident.name.eq_ignore_ascii_case("exp")
                        || ident.name.eq_ignore_ascii_case("abs")
                        || ident.name.eq_ignore_ascii_case("sign")
                        || ident.name.eq_ignore_ascii_case("hypot")
                        || ident.name.eq_ignore_ascii_case("round")
                        || ident.name.eq_ignore_ascii_case("mod")
                        || ident.name.eq_ignore_ascii_case("rem")
                        || ident.name.eq_ignore_ascii_case("atan2")
                        || ident.name.eq_ignore_ascii_case("pow")
                        || ident.name.eq_ignore_ascii_case("log") =>
                {
                    let mut values = Vec::with_capacity(1);
                    loop {
                        match &peek!(self).token {
                            Token::RParen(..) => break,
                            Token::Comma(..) => {
                                values.push(ComponentValue::Delimiter(self.parse()?));
                            }
                            Token::DotDotDot(..)
                                if matches!(self.syntax, Syntax::Scss | Syntax::Sass)
                                    && values.len() == 1 =>
                            {
                                let TokenWithSpan {
                                    span: Span { end, .. },
                                    ..
                                } = bump!(self);
                                let value = values.remove(0);
                                let span = Span {
                                    start: value.span().start,
                                    end,
                                };
                                values.push(ComponentValue::SassArbitraryArgument(
                                    SassArbitraryArgument {
                                        value: Box::new(value),
                                        span,
                                    },
                                ));
                                break;
                            }
                            _ => values.push(self.parse_calc_expr()?),
                        }
                    }
                    values
                }
                InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("element") => {
                    vec![self.parse().map(ComponentValue::IdSelector)?]
                }
                // for IE-specific function `alpha`
                InterpolableIdent::Literal(ident) if ident.name.eq_ignore_ascii_case("alpha") => {
                    self.parse_tokens_in_parens()?
                        .tokens
                        .into_iter()
                        .map(ComponentValue::TokenWithSpan)
                        .collect()
                }
                InterpolableIdent::Literal(Ident {
                    raw: "boolean" | "if",
                    ..
                }) if self.syntax == Syntax::Less => {
                    let condition =
                        ComponentValue::LessCondition(Box::new(self.parse_less_condition(false)?));
                    let mut args = self.parse_function_args()?;
                    args.insert(0, condition);
                    args
                }
                _ => self.parse_function_args()?,
            }
        };
        let end = expect!(self, RParen).1.end;
        let span = Span {
            start: name.span().start,
            end,
        };
        Ok(Function {
            name: FunctionName::Ident(name),
            args,
            span,
        })
    }

    pub(super) fn parse_function_args(&mut self) -> PResult<Vec<ComponentValue<'s>>> {
        let mut values = Vec::with_capacity(4);
        loop {
            match &peek!(self).token {
                Token::RParen(..) | Token::Eof(..) => break,
                Token::Semicolon(..) => {
                    values.push(self.parse().map(ComponentValue::Delimiter)?);
                }
                Token::Exclamation(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    // while this syntax is weird, Bootstrap is actually using it
                    values.push(self.parse().map(ComponentValue::ImportantAnnotation)?);
                }
                Token::LBrace(..) if self.syntax == Syntax::Less => {
                    values.push(self.parse().map(ComponentValue::LessDetachedRuleset)?);
                }
                _ => {
                    let value = self.parse::<ComponentValue>()?;
                    if matches!(self.syntax, Syntax::Scss | Syntax::Sass) {
                        if let Some((_, mut span)) = eat!(self, DotDotDot) {
                            span.start = value.span().start;
                            values.push(ComponentValue::SassArbitraryArgument(
                                SassArbitraryArgument {
                                    value: Box::new(value),
                                    span,
                                },
                            ));
                        } else if let ComponentValue::SassVariable(sass_var) = value {
                            if eat!(self, Colon).is_some() {
                                let value = self.parse::<ComponentValue>()?;
                                let span = Span {
                                    start: sass_var.span.start,
                                    end: value.span().end,
                                };
                                values.push(ComponentValue::SassKeywordArgument(
                                    SassKeywordArgument {
                                        name: sass_var,
                                        value: Box::new(value),
                                        span,
                                    },
                                ));
                            } else {
                                values.push(ComponentValue::SassVariable(sass_var));
                            }
                        } else {
                            values.push(value);
                        }
                    } else {
                        values.push(value);
                    }
                }
            }
        }
        Ok(values)
    }

    pub(super) fn parse_ratio(&mut self, numerator: Number<'s>) -> PResult<Ratio<'s>> {
        expect!(self, Solidus);
        let denominator = self.parse::<Number>()?;
        if denominator.value <= 0.0 {
            self.recoverable_errors.push(Error {
                kind: ErrorKind::InvalidRatioDenominator,
                span: denominator.span.clone(),
            });
        }

        let span = Span {
            start: numerator.span.start,
            end: denominator.span.end,
        };
        Ok(Ratio {
            numerator,
            denominator,
            span,
        })
    }

    fn parse_src_url(&mut self, name: Ident<'s>) -> PResult<Url<'s>> {
        // caller of `parse_src_url` should make sure there're no whitespaces before paren
        expect!(self, LParen);
        let value = match &peek!(self).token {
            Token::Str(..) | Token::StrTemplate(..) => {
                Some(UrlValue::Str(self.parse::<InterpolableStr>()?))
            }
            _ => None,
        };
        let modifiers = match &peek!(self).token {
            Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                let mut modifiers = Vec::with_capacity(1);
                loop {
                    modifiers.push(self.parse()?);
                    if let Token::RParen(..) = &peek!(self).token {
                        break;
                    }
                }
                modifiers
            }
            _ => vec![],
        };
        let end = expect!(self, RParen).1.end;
        let span = Span {
            start: name.span.start,
            end,
        };
        Ok(Url {
            name,
            value,
            modifiers,
            span,
        })
    }

    fn parse_unicode_range(&mut self, prefix_ident: Ident<'s>) -> PResult<UnicodeRange<'s>> {
        let prefix = prefix_ident.raw.chars().next().unwrap();
        let (span_start, span_end) = match bump!(self) {
            TokenWithSpan {
                token: Token::Plus(..),
                span: plus_token_span,
            } => {
                let start = plus_token_span.start;
                let mut end = match self.tokenizer.bump_without_ws_or_comments()? {
                    TokenWithSpan {
                        token: Token::Ident(..) | Token::Question(..),
                        span,
                    } => span.end,
                    TokenWithSpan { token, span } => {
                        return Err(Error {
                            kind: ErrorKind::Unexpected("?", token.symbol()),
                            span,
                        });
                    }
                };
                loop {
                    match peek!(self) {
                        TokenWithSpan {
                            token: Token::Question(..),
                            span,
                        } if span.start == end => {
                            end = bump!(self).span.end;
                        }
                        _ => break,
                    }
                }
                (start, end)
            }
            TokenWithSpan {
                token: Token::Dimension(..),
                span: dimension_token_span,
            } => {
                let start = dimension_token_span.start;
                let mut end = dimension_token_span.end;
                loop {
                    match peek!(self) {
                        TokenWithSpan {
                            token: Token::Question(..),
                            span,
                        } if span.start == end => {
                            end = bump!(self).span.end;
                        }
                        _ => break,
                    }
                }
                (start, end)
            }
            TokenWithSpan {
                token: Token::Number(..),
                span: number_token_span,
            } => {
                let start = number_token_span.start;
                let mut end = number_token_span.end;
                match &peek!(self).token {
                    Token::Question(..) => {
                        end = bump!(self).span.end;
                        loop {
                            match peek!(self) {
                                TokenWithSpan {
                                    token: Token::Question(..),
                                    span,
                                } if span.start == end => {
                                    end = bump!(self).span.end;
                                }
                                _ => break,
                            }
                        }
                    }
                    Token::Dimension(..) | Token::Number(..) => {
                        end = bump!(self).span.end;
                    }
                    _ => {}
                }
                (start, end)
            }
            TokenWithSpan { span, .. } => {
                return Err(Error {
                    kind: ErrorKind::InvalidUnicodeRange,
                    span,
                });
            }
        };

        let source = self.source.get(span_start + 1..span_end).ok_or(Error {
            kind: ErrorKind::InvalidUnicodeRange,
            span: Span {
                start: span_start + 1,
                end: span_end,
            },
        })?;
        let span = Span {
            start: prefix_ident.span.start,
            end: span_end,
        };
        let unicode_range = if let Some((left, right)) = source.split_once('-') {
            if left.len() > 6 || !left.chars().all(|c| c.is_ascii_hexdigit()) {
                return Err(Error {
                    kind: ErrorKind::InvalidUnicodeRange,
                    span,
                });
            }
            if right.len() > 6
                || !right
                    .trim_end_matches('?')
                    .chars()
                    .all(|c| c.is_ascii_hexdigit())
            {
                return Err(Error {
                    kind: ErrorKind::InvalidUnicodeRange,
                    span,
                });
            }
            let start = u32::from_str_radix(left, 16).map_err(|_| Error {
                kind: ErrorKind::InvalidUnicodeRange,
                span: span.clone(),
            })?;
            let end = u32::from_str_radix(&right.replace('?', "F"), 16).map_err(|_| Error {
                kind: ErrorKind::InvalidUnicodeRange,
                span: span.clone(),
            })?;
            UnicodeRange {
                prefix,
                start,
                start_raw: left,
                end,
                end_raw: Some(right),
                span,
            }
        } else {
            if source.len() > 6
                || !source
                    .trim_end_matches('?')
                    .chars()
                    .all(|c| c.is_ascii_hexdigit())
            {
                return Err(Error {
                    kind: ErrorKind::InvalidUnicodeRange,
                    span,
                });
            }
            let start = u32::from_str_radix(&source.replace('?', "0"), 16).map_err(|_| Error {
                kind: ErrorKind::InvalidUnicodeRange,
                span: span.clone(),
            })?;
            let end = u32::from_str_radix(&source.replace('?', "F"), 16).map_err(|_| Error {
                kind: ErrorKind::InvalidUnicodeRange,
                span: span.clone(),
            })?;
            UnicodeRange {
                prefix,
                start,
                start_raw: source,
                end,
                end_raw: None,
                span,
            }
        };
        if unicode_range.end > 0x10ffff {
            self.recoverable_errors.push(Error {
                kind: ErrorKind::MaxCodePointExceeded,
                span: unicode_range.span.clone(),
            });
        }
        if unicode_range.start > unicode_range.end {
            self.recoverable_errors.push(Error {
                kind: ErrorKind::UnicodeRangeStartGreaterThanEnd,
                span: unicode_range.span.clone(),
            });
        }
        Ok(unicode_range)
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for BracketBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let start = expect!(input, LBracket).1.start;
        let mut value = Vec::with_capacity(3);
        loop {
            match &peek!(input).token {
                Token::RBracket(..) => break,
                _ => value.push(input.parse()?),
            }
        }
        let end = expect!(input, RBracket).1.end;
        Ok(BracketBlock {
            value,
            span: Span { start, end },
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComponentValue<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.syntax {
            Syntax::Css => input.parse_component_value_atom(),
            Syntax::Scss | Syntax::Sass => input.parse_sass_bin_expr(),
            Syntax::Less => input.parse_less_operation(/* allow_mixin_call */ true),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ComponentValues<'s> {
    /// This is for public-use only. For internal code of Raffia, **DO NOT** use.
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let first = input.parse::<ComponentValue>()?;
        let mut span = first.span().clone();

        let mut values = Vec::with_capacity(4);
        values.push(first);
        loop {
            match &peek!(input).token {
                Token::Eof(..) => break,
                Token::Semicolon(..) => {
                    values.push(input.parse().map(ComponentValue::Delimiter)?);
                }
                _ => values.push(input.parse()?),
            }
        }

        if let Some(value) = values.last() {
            span.end = value.span().end;
        }
        Ok(ComponentValues { values, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Delimiter {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        use crate::tokenizer::token::*;
        match bump!(input) {
            TokenWithSpan {
                token: Token::Solidus(..),
                span,
            } => Ok(Delimiter {
                kind: DelimiterKind::Solidus,
                span,
            }),
            TokenWithSpan {
                token: Token::Comma(..),
                span,
            } => Ok(Delimiter {
                kind: DelimiterKind::Comma,
                span,
            }),
            TokenWithSpan {
                token: Token::Semicolon(..),
                span,
            } => Ok(Delimiter {
                kind: DelimiterKind::Semicolon,
                span,
            }),
            _ => unreachable!(),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Dimension<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        expect!(input, Dimension).try_into()
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Function<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let name = input.parse::<FunctionName>()?;
        match peek!(input) {
            TokenWithSpan {
                token: Token::LParen(..),
                span,
            } => {
                assert_no_ws_or_comment(name.span(), span)?;
                match name {
                    FunctionName::Ident(name) => input.parse_function(name),
                    FunctionName::SassQualifiedName(name) => {
                        bump!(input);
                        let args = input.parse_function_args()?;
                        let (_, Span { end, .. }) = expect!(input, RParen);
                        let span = Span {
                            start: name.span.start,
                            end,
                        };
                        Ok(Function {
                            name: FunctionName::SassQualifiedName(name),
                            args,
                            span,
                        })
                    }
                }
            }
            TokenWithSpan { token, span } => {
                use crate::{token::LParen, tokenizer::TokenSymbol};
                Err(Error {
                    kind: ErrorKind::Unexpected(LParen::symbol(), token.symbol()),
                    span: span.clone(),
                })
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for FunctionName<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse::<Ident>()?;
        match (&peek!(input).token, input.syntax) {
            (Token::Dot(..), Syntax::Scss | Syntax::Sass) => {
                bump!(input);
                let member = input.parse::<Ident>()?;
                let span = Span {
                    start: ident.span.start,
                    end: member.span.end,
                };
                Ok(FunctionName::SassQualifiedName(SassQualifiedName {
                    module: ident,
                    member: SassModuleMemberName::Ident(member),
                    span,
                }))
            }
            _ => Ok(FunctionName::Ident(InterpolableIdent::Literal(ident))),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for HexColor<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (token, span) = expect!(input, Hash);
        let raw = token.raw;
        let value = if token.escaped {
            handle_escape(raw)
        } else {
            CowStr::from(raw)
        };
        Ok(HexColor { value, raw, span })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Ident<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        Ok(expect!(input, Ident).into())
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for InterpolableIdent<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.syntax {
            Syntax::Css => input.parse().map(InterpolableIdent::Literal),
            Syntax::Scss | Syntax::Sass => input.parse_sass_interpolated_ident(),
            Syntax::Less => {
                // Less variable interpolation is disallowed in declaration value
                if matches!(
                    input.state.qualified_rule_ctx,
                    Some(QualifiedRuleContext::Selector | QualifiedRuleContext::DeclarationName)
                ) {
                    input.parse_less_interpolated_ident()
                } else {
                    input.parse().map(InterpolableIdent::Literal)
                }
            }
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for InterpolableStr<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match peek!(input) {
            TokenWithSpan {
                token: Token::Str(..),
                ..
            } => input.parse().map(InterpolableStr::Literal),
            TokenWithSpan {
                token: Token::StrTemplate(..),
                span,
            } => match input.syntax {
                Syntax::Scss | Syntax::Sass => input.parse().map(InterpolableStr::SassInterpolated),
                Syntax::Less => input.parse().map(InterpolableStr::LessInterpolated),
                Syntax::Css => Err(Error {
                    kind: ErrorKind::UnexpectedTemplateInCss,
                    span: span.clone(),
                }),
            },
            TokenWithSpan { span, .. } => Err(Error {
                kind: ErrorKind::ExpectString,
                span: span.clone(),
            }),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Number<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (number, span) = expect!(input, Number);
        number
            .raw
            .parse()
            .map_err(|_| Error {
                kind: ErrorKind::InvalidNumber,
                span: span.clone(),
            })
            .map(|value| Self {
                value,
                raw: number.raw,
                span,
            })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Percentage<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (token, span) = expect!(input, Percentage);
        Ok(Percentage {
            value: (
                token.value,
                Span {
                    start: span.start,
                    end: span.end - 1,
                },
            )
                .try_into()?,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Str<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (str, span) = expect!(input, Str);
        let raw_without_quotes = unsafe { str.raw.get_unchecked(1..str.raw.len() - 1) };
        let value = if str.escaped {
            handle_escape(raw_without_quotes)
        } else {
            CowStr::from(raw_without_quotes)
        };
        Ok(Str {
            value,
            raw: str.raw,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Url<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (prefix, prefix_span) = expect!(input, Ident);
        if !prefix.name().eq_ignore_ascii_case("url") {
            return Err(Error {
                kind: ErrorKind::ExpectUrl,
                span: prefix_span,
            });
        }

        match peek!(input) {
            TokenWithSpan {
                token: Token::LParen(..),
                span,
            } if prefix_span.end == span.start => {
                bump!(input);
            }
            TokenWithSpan { span, .. } => {
                return Err(Error {
                    kind: ErrorKind::TryParseError,
                    span: span.clone(),
                });
            }
        }

        if let Ok(value) = input.try_parse(InterpolableStr::parse) {
            let modifiers = match &peek!(input).token {
                Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                    let mut modifiers = Vec::with_capacity(1);
                    loop {
                        modifiers.push(input.parse()?);
                        if let Token::RParen(..) = &peek!(input).token {
                            break;
                        }
                    }
                    modifiers
                }
                _ => vec![],
            };
            let end = expect!(input, RParen).1.end;
            let span = Span {
                start: prefix_span.start,
                end,
            };
            Ok(Url {
                name: (prefix, prefix_span).into(),
                value: Some(UrlValue::Str(value)),
                modifiers,
                span,
            })
        } else if let Ok(value) = input.try_parse(UrlRaw::parse) {
            let span = Span {
                start: prefix_span.start,
                end: value.span.end + 1, // `)` is consumed, but span excludes it
            };
            Ok(Url {
                name: (prefix, prefix_span).into(),
                value: Some(UrlValue::Raw(value)),
                modifiers: vec![],
                span,
            })
        } else if matches!(input.syntax, Syntax::Scss | Syntax::Sass) {
            let value = input.parse::<SassInterpolatedUrl>()?;
            let span = Span {
                start: prefix_span.start,
                end: value.span.end + 1, // `)` is consumed, but span excludes it
            };
            Ok(Url {
                name: (prefix, prefix_span).into(),
                value: Some(UrlValue::SassInterpolated(value)),
                modifiers: vec![],
                span,
            })
        } else {
            Err(Error {
                kind: ErrorKind::InvalidUrl,
                span: bump!(input).span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for UrlModifier<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let ident = input.parse::<InterpolableIdent>()?;
        match peek!(input) {
            TokenWithSpan {
                token: Token::LParen(..),
                span,
            } if ident.span().end == span.start => {
                input.parse_function(ident).map(UrlModifier::Function)
            }
            _ => Ok(UrlModifier::Ident(ident)),
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for UrlRaw<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        match input.tokenizer.scan_url_raw_or_template()? {
            TokenWithSpan {
                token: Token::UrlRaw(url),
                span,
            } => {
                let value = if url.escaped {
                    handle_escape(url.raw)
                } else {
                    CowStr::from(url.raw)
                };
                Ok(UrlRaw {
                    value,
                    raw: url.raw,
                    span,
                })
            }
            TokenWithSpan { token, span } => Err(Error {
                kind: ErrorKind::Unexpected("<url>", token.symbol()),
                span,
            }),
        }
    }
}
