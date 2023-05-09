_This changelog only relates to the `raffia` crate._

## Unreleased

- Reject unexpected `@else` at-rule in Sass.
- Added Sass `@at-root` at-rule support.
- Added CSS `An+B of S` syntax support.
- Fixed parsing selector with Less interpolation.
- Added Sass keyword arguments and arbitrary argument support in functions.
- Added Sass-interpolated function name support.
- Added `!global` flag support in Sass variable declaration.
- Added empty arguments support in Sass `@content` at-rule.
- Fixed ambiguous nested rules.
- Fixed parsing comments with `*` char.
- Fixed incorrect Sass nesting declaration parsing.
- Fixed checking invalid ID selector.
- Reject `!default` flag in Sass `@use` at-rule.
- Added qualified rule begins with combinator support in Sass.
- Fixed parsing Sass placeholder selector with interpolation.
- Fixed parsing selector with Sass interpolation.
- Added `!important` in Sass functions support.
- Fixed single hyphen in Sass interpolation.
- Fixed Sass interpolation in custom properties.
- Fixed Sass interpolation in `:nth-*` selector.
- Changed Sass function call AST.
- Fixed parsing Sass `@include` with module name.
- Added support of Sass list without parens.
- Added support of Sass variable declaration with namespace.
- Added support of Sass interpolation in at-rule name.

## v0.2.0

- Added impl `Copy` trait for `Syntax`.
- Added `revert` and `revert-layer` to CSS-wide keywords.
- Fixed parsing Sass `@extend` at-rule.
- Fixed parsing Sass `@forward` at-rule.
- Fixed parsing CSS `@supports` at-rule with function.

## v0.1.0

- Initial release.
