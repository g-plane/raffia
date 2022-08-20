use smallvec::SmallVec;

pub fn is_css_wide_keyword(s: &str) -> bool {
    s.eq_ignore_ascii_case("initial")
        || s.eq_ignore_ascii_case("inherit")
        || s.eq_ignore_ascii_case("unset")
}

/// Return the last element of the given [`SmallVec`].
///
/// Make sure the given [`SmallVec`] is non-empty, otherwise it would lead to Undefined Behavior.
#[inline]
pub fn last_of_non_empty_small_vec<T, const N: usize>(vec: &SmallVec<[T; N]>) -> &T {
    let len = vec.len();
    unsafe { vec.get_unchecked(len - 1) }
}
