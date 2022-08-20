use self::state::{ParserState, QualifiedRuleContext};
use crate::{
    ast::*,
    config::Syntax,
    error::{Error, ErrorKind, PResult},
    pos::{Span, Spanned},
    tokenizer::{token, Token, Tokenizer},
};
pub use builder::ParserBuilder;
use smallvec::SmallVec;

mod at_rule;
mod builder;
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
        match tokenizer.bump()? {
            Token::$variant(token) => token,
            token => {
                return Err(Error {
                    kind: ErrorKind::Unexpected(stringify!($variant), token.symbol()),
                    span: tokenizer.peek()?.span().clone(),
                });
            }
        }
    }};
}

pub trait Parse<'cmt, 's: 'cmt>: Sized {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self>;
}

pub struct Parser<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    tokenizer: Tokenizer<'cmt, 's>,
    state: ParserState,
    recoverable_errors: Vec<Error>,
}

impl<'cmt, 's: 'cmt> Parser<'cmt, 's> {
    pub fn new(source: &'s str, syntax: Syntax) -> Self {
        Parser {
            source,
            syntax: syntax.clone(),
            tokenizer: Tokenizer::new(source, syntax, None),
            state: Default::default(),
            recoverable_errors: vec![],
        }
    }

    pub fn parse<T>(&mut self) -> PResult<T>
    where
        T: Parse<'cmt, 's>,
    {
        T::parse(self)
    }

    pub fn recoverable_errors(&self) -> &[Error] {
        &self.recoverable_errors
    }

    fn try_parse<R, F: Fn(&mut Self) -> PResult<R>>(&mut self, f: F) -> PResult<R> {
        let tokenizer_state = self.tokenizer.state.clone();
        let comments_count = self
            .tokenizer
            .comments
            .as_ref()
            .map(|comments| comments.len());
        let recoverable_errors_count = self.recoverable_errors.len();
        let result = f(self);
        if result.is_err() {
            self.tokenizer.state = tokenizer_state;
            if let Some((comments, count)) = self.tokenizer.comments.as_mut().zip(comments_count) {
                comments.truncate(count);
            }
            self.recoverable_errors.truncate(recoverable_errors_count);
        }
        result
    }

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

    fn parse_simple_block_with<F>(&mut self, f: F) -> PResult<SimpleBlock<'s>>
    where
        F: Fn(&mut Self) -> PResult<Vec<Statement<'s>>>,
    {
        let is_sass = self.syntax == Syntax::Sass;
        let start = if is_sass {
            if let Some(token) = eat!(self, Indent) {
                token.span.end
            } else {
                let offset = self.tokenizer.current_offset();
                return Ok(SimpleBlock {
                    statements: vec![],
                    span: Span {
                        start: offset,
                        end: offset,
                    },
                });
            }
        } else {
            expect!(self, LBrace).span.start
        };

        let statements = f(self)?;

        if is_sass {
            match self.tokenizer.peek()? {
                Token::Dedent(token::Dedent { span }) | Token::Eof(token::Eof { span }) => {
                    self.tokenizer.bump()?;
                    Ok(SimpleBlock {
                        statements,
                        span: Span {
                            start,
                            end: span.start,
                        },
                    })
                }
                token => Err(Error {
                    kind: ErrorKind::ExpectDedentOrEof,
                    span: token.span().clone(),
                }),
            }
        } else {
            let r_brace = expect!(self, RBrace);
            Ok(SimpleBlock {
                statements,
                span: Span {
                    start,
                    end: r_brace.span.end,
                },
            })
        }
    }

    fn parse_statements(&mut self, is_top_level: bool) -> PResult<Vec<Statement<'s>>> {
        let mut statements = Vec::with_capacity(1);
        loop {
            let mut is_block_element = false;
            match self.tokenizer.peek()? {
                Token::Ident(..) | Token::HashLBrace(..) | Token::AtLBraceVar(..) => {
                    if !is_top_level {
                        if let Ok(declaration) = self.try_parse(|parser| parser.parse()) {
                            statements.push(Statement::Declaration(declaration));
                            continue;
                        }
                    }
                    if let Ok(qualified_rule) = self.try_parse(|parser| parser.parse()) {
                        statements.push(Statement::QualifiedRule(qualified_rule));
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
                    statements.push(Statement::QualifiedRule(self.parse()?));
                    is_block_element = true;
                }
                Token::Percent(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    statements.push(Statement::QualifiedRule(self.parse()?));
                    is_block_element = true;
                }
                Token::DollarVar(..) if matches!(self.syntax, Syntax::Scss | Syntax::Sass) => {
                    statements.push(Statement::SassVariableDeclaration(self.parse()?));
                }
                Token::AtKeyword(at_keyword) => {
                    if self.syntax == Syntax::Less {
                        if let Ok(less_variable_declaration) =
                            self.try_parse(|parser| parser.parse())
                        {
                            statements.push(Statement::LessVariableDeclaration(
                                less_variable_declaration,
                            ));
                            continue;
                        }
                    }
                    if matches!(self.syntax, Syntax::Scss | Syntax::Sass) {
                        if let Some(statement) = self.parse_sass_at_rule(&at_keyword.ident.name)? {
                            statements.push(statement);
                            continue;
                        }
                    }
                    let at_rule = self.parse::<AtRule>()?;
                    is_block_element = at_rule.block.is_some();
                    statements.push(Statement::AtRule(at_rule));
                }
                _ => {}
            };
            match self.tokenizer.peek()? {
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

impl<'cmt, 's: 'cmt> Parse<'cmt, 's> for Declaration<'s> {
    fn parse(input: &mut Parser<'cmt, 's>) -> PResult<Self> {
        let name = input
            .with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::DeclarationName),
            })
            .parse::<InterpolableIdent>()?;

        let less_property_merge = if input.syntax == Syntax::Less {
            input.parse()?
        } else {
            None
        };

        let colon = expect!(input, Colon);
        let value = {
            let mut parser = input.with_state(ParserState {
                qualified_rule_ctx: Some(QualifiedRuleContext::DeclarationValue),
            });
            match &name {
                InterpolableIdent::Literal(ident) if ident.name.starts_with("--") => {
                    let mut values = SmallVec::with_capacity(3);
                    loop {
                        match parser.tokenizer.peek()? {
                            Token::RBrace(..)
                            | Token::Semicolon(..)
                            | Token::Dedent(..)
                            | Token::Linebreak(..)
                            | Token::Eof(..) => break,
                            _ => values.push(parser.tokenizer.bump().map(ComponentValue::Token)?),
                        }
                    }
                    values
                }
                _ => {
                    let mut values = SmallVec::with_capacity(3);
                    loop {
                        match parser.tokenizer.peek()? {
                            Token::RBrace(..)
                            | Token::RParen(..)
                            | Token::Semicolon(..)
                            | Token::Dedent(..)
                            | Token::Linebreak(..)
                            | Token::Exclamation(..)
                            | Token::Eof(..) => break,
                            _ => values.push(parser.parse::<ComponentValue>()?),
                        }
                    }
                    values
                }
            }
        };

        let important = if let Token::Exclamation(..) = input.tokenizer.peek()? {
            input.parse::<ImportantAnnotation>().map(Some)?
        } else {
            None
        };

        let span = Span {
            start: name.span().start,
            end: if let Some(important) = &important {
                important.span.end
            } else {
                let last_value: Option<&ComponentValue> = value.last();
                if let Some(last) = last_value {
                    last.span().end
                } else {
                    colon.span.end
                }
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
        let exclamation = expect!(input, Exclamation);
        let ident = expect!(input, Ident);
        input.assert_no_ws_or_comment(&exclamation.span, &ident.span)?;

        let span = Span {
            start: exclamation.span.start,
            end: ident.span.end,
        };
        if ident.name.eq_ignore_ascii_case("important") {
            Ok(ImportantAnnotation {
                ident: ident.into(),
                span,
            })
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
