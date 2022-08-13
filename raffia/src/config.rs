#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub enum Syntax {
    #[default]
    Css,
    Scss,
    Sass,
    Less,
}
