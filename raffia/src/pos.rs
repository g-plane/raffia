#[cfg(feature = "serialize")]
use serde::Serialize;

/// Span represents a range of a piece of source code.
/// It counts by offset, so it's 0-based.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[cfg_attr(feature = "serialize", derive(Serialize))]
pub struct Span {
    /// Start offset. (Inclusive)
    pub start: usize,
    /// End offset. (Exclusive)
    pub end: usize,
}

pub trait Spanned {
    fn span(&self) -> &Span;
}
