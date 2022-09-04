use smallvec::SmallVec;
use std::borrow::Cow;

/// Compare equality of two AST nodes without respecting their spans.
///
/// This allows to compare AST nodes content
/// while they're in different locations of syntax tree.
pub trait SpanIgnoredEq {
    /// Compare equality of two AST nodes without respecting their spans.
    #[must_use]
    fn span_ignored_eq(&self, other: &Self) -> bool;
}

impl SpanIgnoredEq for str {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for &str {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for Cow<'_, str> {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for f64 {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for i32 {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for u32 {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for bool {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl SpanIgnoredEq for char {
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl<T> SpanIgnoredEq for Vec<T>
where
    T: SpanIgnoredEq,
{
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.span_ignored_eq(b))
    }
}

impl<T, const N: usize> SpanIgnoredEq for SmallVec<[T; N]>
where
    T: SpanIgnoredEq,
{
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.span_ignored_eq(b))
    }
}

impl<T> SpanIgnoredEq for Option<T>
where
    T: SpanIgnoredEq,
{
    #[inline]
    fn span_ignored_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.span_ignored_eq(b),
            (None, None) => true,
            _ => false,
        }
    }
}
