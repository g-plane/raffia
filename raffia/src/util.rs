pub fn is_css_wide_keyword(s: &str) -> bool {
    s.eq_ignore_ascii_case("initial")
        || s.eq_ignore_ascii_case("inherit")
        || s.eq_ignore_ascii_case("unset")
}

pub fn trim_vendor_prefix(s: &str) -> &str {
    s.strip_prefix('-')
        .and_then(|s| {
            let trimmed = s.trim_start_matches(|c: char| c.is_ascii_alphanumeric());
            if s == trimmed {
                None
            } else {
                Some(trimmed)
            }
        })
        .and_then(|s| s.strip_prefix('-'))
        .unwrap_or(s)
}
