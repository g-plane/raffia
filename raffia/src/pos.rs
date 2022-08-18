#[cfg(feature = "serialize")]
use serde::Serialize;

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub trait Spanned {
    fn span(&self) -> &Span;
}
