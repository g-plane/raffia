_This changelog only relates to the `raffia` crate._

## Unreleased

- Allowed comments in compound selector.
- Fixed stripping UTF-8 BOM when creating parser with `ParserBuilder`.
- Fixed possible infinite loop when parsing pseudo selector arguments.

## v0.6.0

- Allowed parsing custom property value as normal declaration value.
- Added support of `@scope` at-rule.
- Fixed parsing `@namespace` at-rule.
- Fixed span of Sass `@if` at-rule.
- Added UTF-8 BOM support.
- Changed some AST types with `Box` as wrapper.
- Added token spans for some AST nodes.
- Allowed comments after `:` or `::` in pseudo selectors.
- Allowed comments in type selector.
- Allowed comments in attribute selector name.
- Fixed custom properties in some cases.
- Fixed unknown at-rule prelude.
- Added support for enabling an option to tolerate semicolons in Sass.
- Fixed media query comparison in Sass.
- Fixed parsing nested Sass unary expression.
- Fixed parsing Sass arbitrary argument with comments or whitespaces.
- Allowed tokens in function call.

## v0.5.1

- Rejected whitespace or comments in Sass arbitrary argument.
- Added token spans for some AST nodes.

## v0.5.0

- Changed AST of `@supports` at-rule.
- Changed AST of Sass list.
- Changed AST of most of Sass at-rules.

## v0.4.0

- Added support of `@starting-style` at-rule.
- Changed structure of comments.
- Changed AST of dimension.
- Fixed span when there's a trailing comma in `SelectorList`.
- Changed `Syntax` enum ser/de-able only when `config_serde` enabled.
- Added support of Less.

## v0.3.0

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
- Added support of Sass arbitrary argument in CSS math functions.
- Fixed Sass expression in media query.
- Added support of Sass deprecated `@elseif` syntax.
- Fixed parsing Sass `@include` and `@content` arguments.
- Fixed parsing Sass `@mixin` parameters.
- Fixed parsing Sass `@extend` at-rule.
- Fixed trailing comma in selector in Sass.
- Fixed parsing Sass list in `@each` at-rule.
- Added support of IE `filter` property.
- Fixed parsing Sass interpolation with trailing non-name start.
- Fixed parsing Sass binary plus/minus expression.
- Changed Sass interpolation AST.
- Added support of Sass variable as media feature in `@media` at-rule.
- Fixed some edge cases of `url()`.
- Added support of treating `url()` as function in Sass.
- Added support of single combinator only at selector in Sass.
- Try parsing unknown at-rule prelude as component value.
- Added support of non-standard `:global` pseudo class selector.
- Changed AST of expression in Sass `@return`, `@debug`, `@warn`, `@error` at-rule and module config.
- Added partial support of Sass division.
- Added support of allowing keyframe block in Sass `@mixin` and `@include` at-rule.
- Added support of allowing Sass at-rule in `@keyframes` at-rule.
- Fixed delimiter being calculated in Sass binary expression.
- Added support of deprecated Sass `@import` syntax.
- Fixed Sass statements in `@keyframes` at-rule.
- Changed AST of nesting selector.
- Reject duplicated Sass flag in variable declaration.
- Added support of Less property variable.
- Added support of Less escaped string.
- Added support of Less JavaScript code snippet.

## v0.2.0

- Added impl `Copy` trait for `Syntax`.
- Added `revert` and `revert-layer` to CSS-wide keywords.
- Fixed parsing Sass `@extend` at-rule.
- Fixed parsing Sass `@forward` at-rule.
- Fixed parsing CSS `@supports` at-rule with function.

## v0.1.0

- Initial release.
