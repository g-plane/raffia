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
