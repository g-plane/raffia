#![allow(unused)]

pub use config::Syntax;
pub use parser::parse;
pub use pos::{Span, Spanned};

pub mod ast;
mod config;
pub mod error;
mod parser;
pub mod pos;
mod tokenizer;
