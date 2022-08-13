use super::Parser;
use crate::{
    tokenizer::{token::Comment, Tokenizer},
    Syntax,
};

pub struct ParserBuilder<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    comments: Option<&'cmt mut Vec<Comment<'s>>>,
}

impl<'cmt, 's: 'cmt> ParserBuilder<'cmt, 's> {
    pub fn new(source: &'s str) -> Self {
        ParserBuilder {
            source,
            syntax: Syntax::default(),
            comments: None,
        }
    }

    pub fn syntax(&mut self, syntax: Syntax) -> &Self {
        self.syntax = syntax;
        self
    }

    pub fn comments(&mut self, comments: &'cmt mut Vec<Comment<'s>>) -> &Self {
        self.comments = Some(comments);
        self
    }

    pub fn ignore_comments(&mut self) -> &Self {
        self.comments = None;
        self
    }

    pub fn build(self) -> Parser<'cmt, 's> {
        Parser {
            source: self.source,
            syntax: self.syntax.clone(),
            tokenizer: Tokenizer::new(self.source, self.syntax, self.comments),
            state: Default::default(),
        }
    }
}
