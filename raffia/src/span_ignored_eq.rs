/// Compare equality of two AST nodes without respecting their spans.
///
/// This allows to compare AST nodes content
/// while they're in different locations of syntax tree.
pub trait SpanIgnoredEq {
    /// Compare equality of two AST nodes without respecting their spans.
    #[must_use]
    fn span_ignored_eq(&self, other: &Self) -> bool;
}
