# Raffia

Raffia is parser which can parse CSS, SCSS, Sass (indented syntax) and Less. However, it only parses SCSS, Sass or Less, but it doesn't compile to CSS.

## Example

```rust
use raffia::{ast::Stylesheet, Parser, Syntax};

let mut parser = Parser::new("a { color: green }", Syntax::Css);
let ast = parser.parse::<Stylesheet>().unwrap();
println!("{:#?}", ast);
```

You can find more examples in the [examples](https://github.com/g-plane/raffia/blob/main/raffia/examples) directory.

For detailed usage, check out [docs.rs](https://docs.rs/raffia).

## Current Status

-   CSS: Almost completed.
-   SCSS/Sass: Partially implemented. Some features are still in progress.
-   Less: Only a few features are implemented.

## Credit

Tests come from:

-   [Web Platform Tests](https://github.com/web-platform-tests/wpt)
-   [SWC CSS parser](https://github.com/swc-project/swc/tree/main/crates/swc_css_parser/tests)

## License

MIT License

Copyright (c) 2022-present Pig Fang
