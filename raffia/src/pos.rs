#[derive(Clone, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub trait Spanned {
    fn span(&self) -> &Span;
}
