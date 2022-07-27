use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    pos::{Span, Spanned},
    tokenizer::{Token, Tokenizer},
};

mod less;
mod selector;
mod value;

#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use crate::tokenizer::Token;
        let tokenizer = &mut $parser.tokenizer;
        if let Token::$variant(token) = tokenizer.try_peek()? {
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
                span: tokenizer.peek().span().clone(),
            });
        }
    }};
}

struct Parser<'a> {
    source: &'a str,
    syntax: Syntax,
    tokenizer: Tokenizer<'a>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, syntax: Syntax) -> Self {
        Parser {
            source,
            syntax: syntax.clone(),
            tokenizer: Tokenizer::new(source, syntax),
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

    fn parse_component_values(&mut self) -> PResult<Vec<ComponentValue<'a>>> {
        let mut values = Vec::with_capacity(4);
        loop {
            match self.tokenizer.peek() {
                Token::RBrace(..) | Token::RParen(..) | Token::Semicolon(..) | Token::Eof => {
                    return Ok(values);
                }
                _ => match self.parse_component_value() {
                    Ok(value) => values.push(value),
                    Err(error) => return Err(error),
                },
            }
        }
    }

    fn parse_declaration(&mut self) -> PResult<Declaration<'a>> {
        let name = self.parse_ident()?;
        expect!(self, Colon);
        let value = self.parse_declaration_value()?;
        let span = Span {
            start: name.span.start,
            end: value.span.end,
        };
        Ok(Declaration { name, value, span })
    }

    fn parse_declaration_value(&mut self) -> PResult<DeclarationValue<'a>> {
        let values = self.parse_component_values()?;
        let span = values
            .first()
            .zip(values.last())
            .map(|(first, last)| Span {
                start: first.span().start,
                end: last.span().end,
            })
            .ok_or_else(|| panic!())?;
        Ok(DeclarationValue { values, span })
    }

    fn parse_ident(&mut self) -> PResult<Ident<'a>> {
        Ok(expect!(self, Ident).into())
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
            match self.tokenizer.peek() {
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

    fn parse_str(&mut self) -> PResult<Str<'a>> {
        Ok(expect!(self, Str).into())
    }

    fn parse_stylesheet(&mut self) -> PResult<QualifiedRule<'a>> {
        self.parse_qualified_rule()
    }
}

pub fn parse(source: &str, syntax: Syntax) -> PResult<QualifiedRule> {
    let mut parser = Parser::new(source, syntax);
    parser.parse_stylesheet()
}
