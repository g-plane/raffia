//! Raffia is a parser can parse CSS, SCSS, Sass (indented syntax) and Less.
//!
//! ## Basic Usage
//!
//! This crate provides a simple API to get started.
//!
//! First, create a parser, give it the source code and specify the syntax,
//! then call the [`parse`](Parser::parse) method:
//!
//! ```rust
//! use raffia::{ast::Stylesheet, Parser, Syntax};
//!
//! let mut parser = Parser::new("a {}", Syntax::Css); // syntax can also be `Scss`, `Sass` or `Less`
//! let result = parser.parse::<Stylesheet>();
//! match result {
//!     Ok(ast) => {
//!         // parsed successfully
//!         println!("{:#?}", ast);
//!     }
//!     Err(error) => {
//!         // it failed, error message and position can be accessed via `error`
//!         println!("{:#?}", error);
//!     }
//! }
//! ```
//!
//! ## Advanced Usage
//!
//! ### Creating Parser with Builder
//!
//! If you need to control parser with additional features, you can use [`ParserBuilder`].
//!
//! For example, to collect comments:
//!
//! ```rust
//! use raffia::ParserBuilder;
//!
//! let mut comments = vec![];
//! let builder = ParserBuilder::new("/* comment */ a {}").comments(&mut comments);
//! let mut parser = builder.build();
//! ```
//!
//! By default, syntax is CSS when using parser builder. You can customize it:
//!
//! ```rust
//! use raffia::{ParserBuilder, Syntax};
//!
//! let builder = ParserBuilder::new("a {}").syntax(Syntax::Scss);
//! ```
//!
//! ### Parser Options
//!
//! #### `try_parsing_value_in_custom_property`
//!
//! By default, value of custom property whose name starts with `--` will be parsed as tokens.
//! If you want to parse it as normal declaration value, you can enable this option.
//! Even though this option is enabled,
//! parser will fallback to parse as tokens if there're syntax errors.
//!
//! ```rust
//! use raffia::{ast::*, ParserBuilder, ParserOptions};
//!
//! let options = ParserOptions {
//!     try_parsing_value_in_custom_property: true,
//!     ..Default::default()
//! };
//! let builder = ParserBuilder::new("--foo: calc(var(--bar) + 1px)").options(options);
//! let mut parser = builder.build();
//!
//! let declaration = parser.parse::<Declaration>().unwrap();
//! assert!(matches!(declaration.value[0], ComponentValue::Function(..)));
//! ```
//!
//! ### Parse Partial Structure
//!
//! Sometimes you don't want to parse a full stylesheet.
//! Say you only need to parse a qualified rule or even a single declaration.
//! All you need to do is to update the generics of the [`parse`](Parser::parse) method.
//!
//! ```rust
//! use raffia::{ast::QualifiedRule, Parser, Syntax};
//!
//! let mut parser = Parser::new("a {}", Syntax::Css);
//! parser.parse::<QualifiedRule>();
//! ```
//!
//! and
//!
//! ```rust
//! use raffia::{ast::Declaration, Parser, Syntax};
//!
//! let mut parser = Parser::new("color: green", Syntax::Css);
//! parser.parse::<Declaration>();
//! ```
//!
//! Not all AST nodes support the usage above;
//! technically, those nodes that implement [`Parse`] trait are supported.
//!
//! ### Retrieve Recoverable Errors
//!
//! There may be some recoverable errors which doesn't affect on producing AST.
//! To retrieve those errors, use [`recoverable_errors`](Parser::recoverable_errors).
//!
//! ```rust
//! use raffia::{ast::Stylesheet, Parser, Syntax};
//!
//! let mut parser = Parser::new("@keyframes kf { invalid {} }", Syntax::Css);
//! let result = parser.parse::<Stylesheet>();
//! assert!(result.is_ok());
//! println!("{:?}", parser.recoverable_errors());
//! ```
//!
//! ## Serialization
//!
//! Produced AST can be serialized by Serde, but this feature is disabled by default.
//! You need to enable feature `serialize` manually:
//!
//! ```toml
//! raffia = { version = "*", features = ["serialize"] }
//! ```
//!
//! Then you can pass AST to Serde.
//!
//! Note that Raffia only supports serialization. Deserialization isn't supported.

pub use config::{ParserOptions, Syntax};
pub use parser::{Parse, Parser, ParserBuilder};
pub use pos::{Span, Spanned};
pub use span_ignored_eq::SpanIgnoredEq;
pub use tokenizer::token;

pub mod ast;
mod config;
pub mod error;
mod parser;
pub mod pos;
mod span_ignored_eq;
mod tokenizer;
mod util;
