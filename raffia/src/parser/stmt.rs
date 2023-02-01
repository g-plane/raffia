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
                InterpolableIdent::Literal(ident) if ident.name.starts_with("--") => {
                    let mut values = Vec::with_capacity(3);
                    let mut pairs = Vec::with_capacity(1);
                    loop {
                        match &peek!(parser).token {
                            Token::RBrace(..)
                            | Token::Semicolon(..)
                            | Token::Dedent(..)
                            | Token::Linebreak(..)
                            | Token::Eof(..) => break,
                            _ => {
                                match &peek!(parser).token {
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
                                values.push(ComponentValue::TokenWithSpan(bump!(parser)));
                            }
                        }
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
        input.parse_simple_block_with(|parser| {
            parser.parse_statements(/* is_top_level */ false)
        })
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
    pub(super) fn parse_simple_block_with<F>(&mut self, f: F) -> PResult<SimpleBlock<'s>>
    where
        F: Fn(&mut Self) -> PResult<Vec<Statement<'s>>>,
    {
        let is_sass = self.syntax == Syntax::Sass;
        let start = if is_sass {
            if let Some((_, span)) = eat!(self, Indent) {
                span.end
            } else {
                let offset = peek!(self).span.start;
                return Ok(SimpleBlock {
                    statements: vec![],
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                });
            }
        } else {
            expect!(self, LBrace).1.start
        };

        let statements = f(self)?;

        if is_sass {
            match peek!(self) {
                TokenWithSpan {
                    token: Token::Dedent(..) | Token::Eof(..),
                    span,
                } => {
                    let end = span.start;
                    bump!(self);
                    Ok(SimpleBlock {
                        statements,
                        span: Span { start, end },
                    })
                }
                token => Err(Error {
                    kind: ErrorKind::ExpectDedentOrEof,
                    span: token.span().clone(),
                }),
            }
        } else {
            let end = expect!(self, RBrace).1.end;
            Ok(SimpleBlock {
                statements,
                span: Span { start, end },
            })
        }
    }

    fn parse_statements(&mut self, is_top_level: bool) -> PResult<Vec<Statement<'s>>> {
        let mut statements = Vec::with_capacity(1);
        loop {
            let mut is_block_element = false;
            match &peek!(self).token {
                Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                    if is_top_level {
                        statements.push(Statement::QualifiedRule(self.parse()?));
                        is_block_element = true;
                    } else {
                        match self.try_parse(Declaration::parse) {
                            Ok(declaration) => {
                                is_block_element = matches!(
                                    declaration.value.last(),
                                    Some(ComponentValue::SassNestingDeclaration(..))
                                );
                                statements.push(Statement::Declaration(declaration));
                            }
                            Err(e) => {
                                if let Ok(rule) = self.parse() {
                                    statements.push(Statement::QualifiedRule(rule));
                                    is_block_element = true;
                                } else {
                                    // using the error from parsing declaration for better error message
                                    return Err(e);
                                }
                            }
                        }
                    }
                }
                Token::Dot(..)
                | Token::Hash(..)
                | Token::Ampersand(..)
                | Token::LBracket(..)
                | Token::Colon(..)
                | Token::ColonColon(..)
                | Token::Asterisk(..)
                | Token::Bar(..) => {
                    statements.push(Statement::QualifiedRule(self.parse()?));
                    is_block_element = true;
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
                        if let Ok(less_variable_declaration) =
                            self.try_parse(|parser| parser.parse())
                        {
                            statements.push(Statement::LessVariableDeclaration(
                                less_variable_declaration,
                            ));
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
                Token::Cdo(..) | Token::Cdc(..) => {
                    bump!(self);
                    continue;
                }
                _ => {}
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
