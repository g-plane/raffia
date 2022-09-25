#[cfg(feature = "serialize")]
use serde::{Deserialize, Serialize};

/// Supported syntax.
#[derive(Clone, Debug, Default, PartialEq, Eq, Copy)]
#[cfg_attr(feature = "serialize", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "serialize", serde(rename_all = "camelCase"))]
pub enum Syntax {
    #[default]
    Css,
    Scss,
    /// Indented Sass Syntax
    Sass,
    Less,
}
