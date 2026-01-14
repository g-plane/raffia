use super::Parser;
use crate::{
    ParserOptions, Syntax,
    tokenizer::{Tokenizer, token::Comment},
};

/// Parser builder is for building a parser while allowing us
/// to control advanced behaviors.
///
/// Unlike [`Parser`], syntax isn't required when creating a parser builder,
/// and the default syntax will be CSS. If you need to parse with another syntax,
/// use the [`syntax`](ParserBuilder::syntax) to modify it.
pub struct ParserBuilder<'cmt, 's: 'cmt> {
    source: &'s str,
    syntax: Syntax,
    options: Option<ParserOptions>,
    comments: Option<&'cmt mut Vec<Comment<'s>>>,
}

impl<'cmt, 's: 'cmt> ParserBuilder<'cmt, 's> {
    /// Create a parser builder from given source code.
    pub fn new(source: &'s str) -> Self {
        let source = source.strip_prefix('\u{feff}').unwrap_or(source);
        ParserBuilder {
            source,
            options: None,
            syntax: Syntax::default(),
            comments: None,
        }
    }

    /// Specify the syntax for parsing.
    pub fn syntax(mut self, syntax: Syntax) -> Self {
        self.syntax = syntax;
        self
    }

    /// Customize parser options.
    pub fn options(mut self, options: ParserOptions) -> Self {
        self.options = Some(options);
        self
    }

    /// Collect comments and put them into the given collection.
    pub fn comments(mut self, comments: &'cmt mut Vec<Comment<'s>>) -> Self {
        self.comments = Some(comments);
        self
    }

    /// Disable collecting comments.
    ///
    /// Collecting comments is disabled by default,
    /// so you don't need to use this if you never call the [`comments`](ParserBuilder::comments) method.
    pub fn ignore_comments(mut self) -> Self {
        self.comments = None;
        self
    }

    /// Build a parser.
    pub fn build(self) -> Parser<'cmt, 's> {
        Parser {
            source: self.source,
            syntax: self.syntax,
            options: self.options.unwrap_or_default(),
            tokenizer: Tokenizer::new(self.source, self.syntax, self.comments),
            state: Default::default(),
            recoverable_errors: vec![],
            cached_token: None,
        }
    }
}
