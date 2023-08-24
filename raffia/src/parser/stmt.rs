use super::{
    state::{ParserState, QualifiedRuleContext},
    Parser,
};
use crate::{
    ast::*,
    bump, eat,
    error::{Error, ErrorKind, PResult},
    expect, peek,
    pos::{Span, Spanned},
    tokenizer::{Token, TokenWithSpan},
    util::PairedToken,
    Parse, Syntax,
};

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Declaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let name = input
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::DeclarationName),
                ..input.state
            })
            .parse::<InterpolableIdent>()?;

        let less_property_merge = if input.syntax == Syntax::Less {
            input.parse()?
        } else {
            None
        };

        let (_, colon_span) = expect!(input, Colon);
        let value = {
            let mut parser = input.with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::DeclarationValue),
                ..input.state
            });
            match &name {
                InterpolableIdent::Literal(ident)
                    if ident.name.starts_with("--")
                        || ident.name.eq_ignore_ascii_case("filter")
                            && matches!(
                                &peek!(parser).token,
                                // for IE-compatibility:
                                // filter: progid:DXImageTransform.Microsoft...
                                Token::Ident(ident) if ident.name().eq_ignore_ascii_case("progid")
                            ) =>
                {
                    let mut values = Vec::with_capacity(3);
                    let mut pairs = Vec::with_capacity(1);
                    loop {
                        match &peek!(parser).token {
                            Token::Semicolon(..)
                            | Token::Dedent(..)
                            | Token::Linebreak(..)
                            | Token::Eof(..) => break,
                            Token::LParen(..) => {
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
                            Token::LBrace(..) | Token::HashLBrace(..) => {
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
                        values.push(ComponentValue::TokenWithSpan(bump!(parser)));
                    }
                    values
                }
                _ => {
                    let mut values = Vec::with_capacity(3);
                    loop {
                        match &peek!(parser).token {
                            Token::RBrace(..)
                            | Token::RParen(..)
                            | Token::Semicolon(..)
                            | Token::Dedent(..)
                            | Token::Linebreak(..)
                            | Token::Exclamation(..)
                            | Token::Eof(..) => break,
                            _ => {
                                let value = parser.parse::<ComponentValue>()?;
                                match &value {
                                    ComponentValue::SassNestingDeclaration(..)
                                        if matches!(parser.syntax, Syntax::Scss | Syntax::Sass) =>
                                    {
                                        values.push(value);
                                        break;
                                    }
                                    _ => values.push(value),
                                }
                            }
                        }
                    }
                    values
                }
            }
        };

        let important = if let Token::Exclamation(..) = &peek!(input).token {
            input.parse::<ImportantAnnotation>().map(Some)?
        } else {
            None
        };

        let span = Span {
            start: name.span().start,
            end: if let Some(important) = &important {
                important.span.end
            } else if let Some(last) = value.last() {
                last.span().end
            } else {
                colon_span.end
            },
        };
        Ok(Declaration {
            name,
            value,
            important,
            less_property_merge,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for ImportantAnnotation<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let (_, span) = expect!(input, Exclamation);
        let ident: Ident = input.parse::<Ident>()?;
        let span = Span {
            start: span.start,
            end: ident.span.end,
        };
        if ident.name.eq_ignore_ascii_case("important") {
            Ok(ImportantAnnotation { ident, span })
        } else {
            Err(Error {
                kind: ErrorKind::ExpectImportantAnnotation,
                span,
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for QualifiedRule<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let selector_list = input
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::Selector),
                ..input.state
            })
            .parse::<SelectorList>()?;
        let block = input.parse::<SimpleBlock>()?;
        let span = Span {
            start: selector_list.span.start,
            end: block.span.end,
        };
        Ok(QualifiedRule {
            selector: selector_list,
            block,
            span,
        })
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for SimpleBlock<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let is_sass = input.syntax == Syntax::Sass;
        let start = if is_sass {
            if let Some((_, span)) = eat!(input, Indent) {
                span.end
            } else {
                let offset = peek!(input).span.start;
                return Ok(SimpleBlock {
                    statements: vec![],
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                });
            }
        } else {
            expect!(input, LBrace).1.start
        };

        let statements = input.parse_statements(/* is_top_level */ false)?;

        if is_sass {
            match bump!(input) {
                TokenWithSpan {
                    token: Token::Dedent(..) | Token::Eof(..),
                    span,
                } => {
                    let end = span.start;
                    Ok(SimpleBlock {
                        statements,
                        span: Span { start, end },
                    })
                }
                TokenWithSpan { span, .. } => Err(Error {
                    kind: ErrorKind::ExpectDedentOrEof,
                    span,
                }),
            }
        } else {
            let end = expect!(input, RBrace).1.end;
            Ok(SimpleBlock {
                statements,
                span: Span { start, end },
            })
        }
    }
}

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Stylesheet<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        if input.syntax == Syntax::Sass {
            eat!(input, Linebreak);
        }

        let statements = input.parse_statements(/* is_top_level */ true)?;
        expect!(input, Eof);
        Ok(Stylesheet {
            statements,
            span: Span {
                start: 0,
                end: input.source.len(),
            },
        })
    }
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn parse_statements(&mut self, is_top_level: bool) -> PResult<Vec<Statement<'s>>> {
        let mut statements = Vec::with_capacity(1);
        loop {
            let mut is_block_element = false;
            let TokenWithSpan { token, span } = peek!(self);
            match token {
                Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                    match self.syntax {
                        Syntax::Css => {
                            if is_top_level {
                                statements.push(Statement::QualifiedRule(self.parse()?));
                                is_block_element = true;
                            } else if self.state.in_keyframes_at_rule {
                                statements.push(Statement::KeyframeBlock(self.parse()?));
                                is_block_element = true;
                            } else if let Ok(rule) = self.try_parse(QualifiedRule::parse) {
                                statements.push(Statement::QualifiedRule(rule));
                                is_block_element = true;
                            } else {
                                statements.push(Statement::Declaration(self.parse()?));
                            }
                        }
                        Syntax::Scss | Syntax::Sass => {
                            if let Ok(sass_var_decl) =
                                self.try_parse(SassVariableDeclaration::parse)
                            {
                                statements.push(Statement::SassVariableDeclaration(sass_var_decl));
                            } else if is_top_level {
                                statements.push(Statement::QualifiedRule(self.parse()?));
                                is_block_element = true;
                            } else if self.state.in_keyframes_at_rule {
                                statements.push(Statement::KeyframeBlock(self.parse()?));
                                is_block_element = true;
                            } else if let Ok(rule) = self.try_parse(QualifiedRule::parse) {
                                statements.push(Statement::QualifiedRule(rule));
                                is_block_element = true;
                            } else {
                                let decl = self.parse::<Declaration>()?;
                                is_block_element = matches!(
                                    decl.value.last(),
                                    Some(ComponentValue::SassNestingDeclaration(..))
                                );
                                statements.push(Statement::Declaration(decl));
                            }
                        }
                        Syntax::Less => {
                            if is_top_level {
                                statements.push(self.parse_less_qualified_rule()?);
                                is_block_element = true;
                            } else if self.state.in_keyframes_at_rule {
                                statements.push(Statement::KeyframeBlock(self.parse()?));
                                is_block_element = true;
                            } else if let Ok(stmt) =
                                self.try_parse(Parser::parse_less_qualified_rule)
                            {
                                statements.push(stmt);
                                is_block_element = true;
                            } else if let Ok(decl) = self.try_parse(Declaration::parse) {
                                statements.push(Statement::Declaration(decl));
                            } else {
                                statements.push(Statement::LessFunctionCall(self.parse()?));
                            }
                        }
                    }
                }
                Token::Dot(..) | Token::Hash(..) if !self.state.in_keyframes_at_rule => {
                    if self.syntax == Syntax::Less {
                        let stmt = if let Ok(stmt) =
                            self.try_parse(Parser::parse_less_qualified_rule)
                        {
                            is_block_element = true;
                            stmt
                        } else if let Ok(mixin_def) = self.try_parse(LessMixinDefinition::parse) {
                            is_block_element = true;
                            Statement::LessMixinDefinition(mixin_def)
                        } else {
                            self.parse().map(Statement::LessMixinCall)?
                        };
                        statements.push(stmt);
                    } else {
                        statements.push(Statement::QualifiedRule(self.parse()?));
                        is_block_element = true;
                    }
                }
                Token::Ampersand(..)
                | Token::LBracket(..)
                | Token::Colon(..)
                | Token::ColonColon(..)
                | Token::Asterisk(..)
                | Token::Bar(..)
                | Token::NumberSign(..)
                    if !self.state.in_keyframes_at_rule =>
                {
                    if self.syntax == Syntax::Less {
                        if let Ok(extend_rule) = self.try_parse(LessExtendRule::parse) {
                            statements.push(Statement::LessExtendRule(extend_rule));
                        } else {
                            statements.push(self.parse_less_qualified_rule()?);
                            is_block_element = true;
                        }
                    } else {
                        statements.push(Statement::QualifiedRule(self.parse()?));
                        is_block_element = true;
                    }
                }
                Token::AtKeyword(at_keyword) => match self.syntax {
                    Syntax::Css => {
                        let at_rule = self.parse::<AtRule>()?;
                        is_block_element = at_rule.block.is_some();
                        statements.push(Statement::AtRule(at_rule));
                    }
                    Syntax::Scss | Syntax::Sass => {
                        let at_keyword_name = at_keyword.ident.name();
                        if let Some((statement, is_block)) =
                            self.parse_sass_at_rule(&at_keyword_name)?
                        {
                            statements.push(statement);
                            is_block_element = is_block;
                        } else {
                            let at_rule = self.parse::<AtRule>()?;
                            is_block_element = at_rule.block.is_some();
                            statements.push(Statement::AtRule(at_rule));
                        }
                    }
                    Syntax::Less => {
                        let at_keyword_name = at_keyword.ident.name();
                        if let Ok(less_variable_declaration) =
                            self.try_parse(LessVariableDeclaration::parse)
                        {
                            is_block_element = less_variable_declaration
                                .value
                                .values
                                .last()
                                .map(ComponentValue::is_less_detached_ruleset)
                                .unwrap_or_default();
                            statements.push(Statement::LessVariableDeclaration(
                                less_variable_declaration,
                            ));
                        } else if let Ok(variable_call) = self.try_parse(LessVariableCall::parse) {
                            statements.push(Statement::LessVariableCall(variable_call));
                        } else if let Some((statement, is_block)) =
                            self.parse_less_at_rule(&at_keyword_name)?
                        {
                            statements.push(statement);
                            is_block_element = is_block;
                        } else {
                            let at_rule = self.parse::<AtRule>()?;
                            is_block_element = at_rule.block.is_some();
                            statements.push(Statement::AtRule(at_rule));
                        }
                    }
                },
                Token::Percent(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    statements.push(Statement::QualifiedRule(self.parse()?));
                    is_block_element = true;
                }
                Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    statements.push(Statement::SassVariableDeclaration(self.parse()?));
                }
                Token::GreaterThan(..) | Token::Plus(..) | Token::Tilde(..) | Token::BarBar(..)
                    if matches!(self.syntax, Syntax::Scss | Syntax::Sass) =>
                {
                    statements.push(Statement::QualifiedRule(self.parse()?));
                    is_block_element = true;
                }
                Token::GreaterThan(..) | Token::Plus(..) | Token::Tilde(..) | Token::BarBar(..)
                    if self.syntax == Syntax::Less =>
                {
                    statements.push(self.parse_less_qualified_rule()?);
                    is_block_element = true;
                }
                Token::DollarLBraceVar(..) if self.syntax == Syntax::Less => {
                    statements.push(self.parse().map(Statement::Declaration)?);
                }
                Token::Cdo(..) | Token::Cdc(..) => {
                    bump!(self);
                    continue;
                }
                Token::At(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    let unknown_sass_at_rule = self.parse::<UnknownSassAtRule>()?;
                    is_block_element = unknown_sass_at_rule.block.is_some();
                    statements.push(Statement::UnknownSassAtRule(unknown_sass_at_rule));
                }
                Token::Percentage(..)
                    if self.state.in_keyframes_at_rule
                        || self.state.sass_ctx & super::state::SASS_CTX_ALLOW_KEYFRAME_BLOCK
                            != 0 =>
                {
                    statements.push(Statement::KeyframeBlock(self.parse()?));
                    is_block_element = true;
                }
                Token::RBrace(..) | Token::Eof(..) | Token::Dedent(..) => break,
                Token::Semicolon(..) | Token::Linebreak(..) => {
                    bump!(self);
                    continue;
                }
                _ => {
                    return Err(Error {
                        kind: if self.state.in_keyframes_at_rule {
                            ErrorKind::ExpectKeyframeBlock
                        } else {
                            ErrorKind::ExpectRule
                        },
                        span: span.clone(),
                    });
                }
            };
            match &peek!(self).token {
                Token::RBrace(..) | Token::Eof(..) | Token::Dedent(..) => break,
                _ => {
                    if self.syntax == Syntax::Sass {
                        if is_block_element {
                            eat!(self, Linebreak);
                        } else {
                            expect!(self, Linebreak);
                        }
                    } else if is_block_element {
                        eat!(self, Semicolon);
                    } else {
                        expect!(self, Semicolon);
                    }
                }
            }
        }
        Ok(statements)
    }
}
