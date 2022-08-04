use self::state::{ParserState, QualifiedRuleContext};
use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    pos::{Span, Spanned},
    tokenizer::{
        token::{self, Comment},
        Token, Tokenizer,
    },
};

mod less;
mod sass;
mod selector;
mod state;
mod value;

#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use $crate::tokenizer::Token;
        let tokenizer = &mut $parser.tokenizer;
        if let Token::$variant(token) = tokenizer.peek()? {
            let _ = tokenizer.bump();
            Some(token)
        } else {
            None
        }
    }};
}

#[macro_export]
macro_rules! expect {
    ($parser:expr, $variant:ident) => {{
        use $crate::{
            error::{Error, ErrorKind},
            tokenizer::Token,
        };
        let tokenizer = &mut $parser.tokenizer;
        if let Token::$variant(token) = tokenizer.bump()? {
            token
        } else {
            return Err(Error {
                kind: ErrorKind::Unexpected(stringify!($variant)),
                span: tokenizer.peek()?.span().clone(),
            });
        }
    }};
}

struct Parser<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    tokenizer: Tokenizer<'cmt, 's>,
    state: ParserState,
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    fn new(source: &'s str, syntax: Syntax, comments: Option<&'cmt mut Vec<Comment<'s>>>) -> Self {
        Parser {
            source,
            syntax: syntax.clone(),
            tokenizer: Tokenizer::new(source, syntax, comments),
            state: Default::default(),
        }
    }

    fn try_parse<R, F: Fn(&mut Self) -> PResult<R>>(&mut self, f: F) -> Option<R> {
        let tokenizer_state = self.tokenizer.clone_state();
        let comments_count = self
            .tokenizer
            .comments
            .as_ref()
            .map(|comments| comments.len());
        match f(self) {
            Ok(value) => Some(value),
            Err(..) => {
                self.tokenizer.replace_state(tokenizer_state);
                if let Some((comments, count)) =
                    self.tokenizer.comments.as_mut().zip(comments_count)
                {
                    comments.truncate(count);
                }
                None
            }
        }
    }

    fn parse_declaration(&mut self) -> PResult<Declaration<'s>> {
        let name = self
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::DeclarationName),
            })
            .parse_interpolable_ident()?;

        let less_property_merge = if self.syntax == Syntax::Less {
            self.parse_less_property_merge()?
        } else {
            None
        };

        expect!(self, Colon);
        let value = self
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::DeclarationValue),
            })
            .parse_component_values(/* allow_comma */ true)?;

        let span = Span {
            start: name.span().start,
            end: value.span.end,
        };
        Ok(Declaration {
            name,
            value,
            less_property_merge,
            span,
        })
    }

    fn parse_qualified_rule(&mut self) -> PResult<QualifiedRule<'s>> {
        let selector_list = self
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::Selector),
            })
            .parse_selector_list()?;
        let block = self.parse_simple_block()?;
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

    fn parse_simple_block(&mut self) -> PResult<SimpleBlock<'s>> {
        let start = if self.syntax == Syntax::Sass {
            if let Some(token) = eat!(self, Indent) {
                token.span.end
            } else {
                let offset = self.tokenizer.current_offset();
                return Ok(SimpleBlock {
                    elements: vec![],
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                });
            }
        } else {
            expect!(self, LBrace).span.start
        };

        let mut elements = Vec::with_capacity(4);
        loop {
            let mut is_block_element = false;
            match self.tokenizer.peek()? {
                Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                    if let Some(declaration) = self.try_parse(|parser| parser.parse_declaration()) {
                        elements.push(SimpleBlockElement::Declaration(declaration));
                    } else if let Some(qualified_rule) =
                        self.try_parse(|parser| parser.parse_qualified_rule())
                    {
                        elements.push(SimpleBlockElement::QualifiedRule(qualified_rule));
                        is_block_element = true;
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
                    elements.push(SimpleBlockElement::QualifiedRule(
                        self.parse_qualified_rule()?,
                    ));
                    is_block_element = true;
                }
                Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    elements.push(SimpleBlockElement::SassVariableDeclaration(
                        self.parse_sass_variable_declaration()?,
                    ));
                }
                Token::AtKeyword(..) => {
                    if self.syntax == Syntax::Less {
                        if let Some(less_variable_declaration) =
                            self.try_parse(|parser| parser.parse_less_variable_declaration())
                        {
                            elements.push(SimpleBlockElement::LessVariableDeclaration(
                                less_variable_declaration,
                            ));
                        }
                    }
                }
                _ => {}
            }
            if self.syntax == Syntax::Sass {
                match self.tokenizer.peek()? {
                    Token::Dedent(token::Dedent { span }) | Token::Eof(token::Eof { span }) => {
                        self.tokenizer.bump()?;
                        return Ok(SimpleBlock {
                            elements,
                            span: Span {
                                start,
                                end: span.start,
                            },
                        });
                    }
                    _ => {
                        if is_block_element {
                            eat!(self, Linebreak);
                        } else {
                            expect!(self, Linebreak);
                        }
                    }
                }
            } else if let Some(r_brace) = eat!(self, RBrace) {
                return Ok(SimpleBlock {
                    elements,
                    span: Span {
                        start,
                        end: r_brace.span.end,
                    },
                });
            } else if is_block_element {
                eat!(self, Semicolon);
            } else {
                expect!(self, Semicolon);
            }
        }
    }

    fn parse_stylesheet(&mut self) -> PResult<Stylesheet<'s>> {
        if self.syntax == Syntax::Sass {
            eat!(self, Linebreak);
        }

        let mut statements = Vec::with_capacity(4);

        loop {
            let mut is_block_element = false;
            match self.tokenizer.peek()? {
                Token::Ident(..)
                | Token::Dot(..)
                | Token::Hash(..)
                | Token::Ampersand(..)
                | Token::LBracket(..)
                | Token::Colon(..)
                | Token::ColonColon(..)
                | Token::Asterisk(..)
                | Token::HashLBrace(..)
                | Token::NumberSign(..)
                | Token::Bar(..) => {
                    statements.push(TopLevelStatement::QualifiedRule(
                        self.parse_qualified_rule()?,
                    ));
                    is_block_element = true;
                }
                Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    statements.push(TopLevelStatement::SassVariableDeclaration(
                        self.parse_sass_variable_declaration()?,
                    ));
                }
                Token::AtKeyword(..) => {
                    if self.syntax == Syntax::Less {
                        if let Some(less_variable_declaration) =
                            self.try_parse(|parser| parser.parse_less_variable_declaration())
                        {
                            statements.push(TopLevelStatement::LessVariableDeclaration(
                                less_variable_declaration,
                            ));
                        }
                    }
                }
                _ => {}
            }
            if eat!(self, Eof).is_some() {
                break;
            } else if is_block_element {
                eat!(self, Semicolon);
            } else {
                expect!(self, Semicolon);
            }
        }

        Ok(Stylesheet {
            statements,
            span: Span {
                start: 0,
                end: self.source.len(),
            },
        })
    }
}

pub fn parse(source: &str, syntax: Syntax) -> PResult<Stylesheet> {
    let mut parser = Parser::new(source, syntax, None);
    parser.parse_stylesheet()
}

pub fn parse_with_comments(source: &str, syntax: Syntax) -> PResult<(Stylesheet, Vec<Comment>)> {
    let mut comments = vec![];
    let mut parser = Parser::new(source, syntax, Some(&mut comments));
    parser.parse_stylesheet().map(|ast| (ast, comments))
}
