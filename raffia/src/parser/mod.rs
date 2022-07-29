use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    pos::{Span, Spanned},
    tokenizer::{Token, Tokenizer},
};

mod less;
mod sass;
mod selector;
mod value;

#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use crate::tokenizer::Token;
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
        use crate::{
            error::{Error, ErrorKind},
            tokenizer::Token,
        };
        let tokenizer = &mut $parser.tokenizer;
        if let Token::$variant(token) = tokenizer.bump()? {
            token
        } else {
            return Err(Error {
                kind: ErrorKind::Unexpected,
                span: tokenizer.peek()?.span().clone(),
            });
        }
    }};
}

#[derive(Clone, Debug, Default)]
struct ParserState {}

struct Parser<'a> {
    source: &'a str,
    syntax: Syntax,
    tokenizer: Tokenizer<'a>,
    state: ParserState,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, syntax: Syntax) -> Self {
        Parser {
            source,
            syntax: syntax.clone(),
            tokenizer: Tokenizer::new(source, syntax),
            state: Default::default(),
        }
    }

    fn try_parse<R, F: Fn(&mut Self) -> PResult<R>>(&mut self, f: F) -> Option<R> {
        let tokenizer_state = self.tokenizer.clone_state();
        match f(self) {
            Ok(value) => Some(value),
            Err(..) => {
                self.tokenizer.replace_state(tokenizer_state);
                None
            }
        }
    }

    fn with_state<R, F: Fn(&mut Self) -> PResult<R>>(
        &mut self,
        state: ParserState,
        f: F,
    ) -> PResult<R> {
        let original_state = self.state.clone();
        let result = f(self);
        self.state = original_state;
        result
    }

    fn parse_declaration(&mut self) -> PResult<Declaration<'a>> {
        let name = self.parse_interpolable_ident()?;
        expect!(self, Colon);
        let value = self.parse_declaration_value()?;
        let span = Span {
            start: name.span().start,
            end: value.span.end,
        };
        Ok(Declaration { name, value, span })
    }

    fn parse_declaration_value(&mut self) -> PResult<DeclarationValue<'a>> {
        let (values, span) = self.parse_component_values()?;
        Ok(DeclarationValue { values, span })
    }

    fn parse_qualified_rule(&mut self) -> PResult<QualifiedRule<'a>> {
        let selector_list = self.parse_selector_list()?;
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

    fn parse_simple_block(&mut self) -> PResult<SimpleBlock<'a>> {
        let l_brace = expect!(self, LBrace);

        let mut elements = Vec::with_capacity(4);
        loop {
            let mut is_block_element = false;
            match self.tokenizer.peek()? {
                Token::Ident(..) => {
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
                Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss) => {
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
            if let Some(r_brace) = eat!(self, RBrace) {
                return Ok(SimpleBlock {
                    elements,
                    span: Span {
                        start: l_brace.span.start,
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

    fn parse_stylesheet(&mut self) -> PResult<Stylesheet<'a>> {
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
                | Token::Bar(..) => {
                    statements.push(TopLevelStatement::QualifiedRule(
                        self.parse_qualified_rule()?,
                    ));
                    is_block_element = true;
                }
                Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss) => {
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
            if let Token::Eof = self.tokenizer.peek()? {
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
    let mut parser = Parser::new(source, syntax);
    parser.parse_stylesheet()
}
