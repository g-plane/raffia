/// Supported syntax.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Syntax {
    #[default]
    Css,
    Scss,
    /// Indented Sass Syntax
    Sass,
    Less,
}
