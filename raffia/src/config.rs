#[cfg(feature = "serialize")]
use serde::Serialize;

/// Supported syntax.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
#[cfg_attr(feature = "serialize", serde(rename_all = "camelCase"))]
pub enum Syntax {
    #[default]
    Css,
    Scss,
    /// Indented Sass Syntax
    Sass,
    Less,
}
