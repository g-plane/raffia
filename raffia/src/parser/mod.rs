use crate::{
    ast::*,
    config::Syntax,
    error::PResult,
    pos::{Span, Spanned},
    tokenizer::{Token, Tokenizer},
};

mod selector;
mod value;

#[macro_export]
macro_rules! eat {
    ($parser:expr, $variant:ident) => {{
        use crate::tokenizer::Token;
        let tokenizer = &mut $parser.tokenizer;
        if let Token::$variant(token) = tokenizer.peek() {
            tokenizer.bump()?;
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
            match self.tokenizer.peek() {
                Token::Ident(..) => {
                    elements.push(SimpleBlockElement::Declaration(self.parse_declaration()?))
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

pub fn parse(source: &str) -> PResult<QualifiedRule> {
    let mut parser = Parser::new(source, Syntax::Css);
    parser.parse_stylesheet()
}
