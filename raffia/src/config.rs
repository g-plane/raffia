#[cfg(feature = "config_serde")]
use serde::{Deserialize, Serialize};

/// Supported syntax.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "config_serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "config_serde", serde(rename_all = "camelCase"))]
pub enum Syntax {
    #[default]
    Css,
    Scss,
    /// Indented Sass Syntax
    Sass,
    Less,
}

/// Parser options for customizing parser behaviors.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "config_serde", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "config_serde", serde(rename_all = "camelCase"))]
pub struct ParserOptions {
    /// Enabling this will make parser attempt to parse
    /// custom property value as normal declaration value instead of tokens.
    /// It will fallback to parse as tokens if there're syntax errors
    /// when parsing as values.
    pub try_parsing_value_in_custom_property: bool,
}
