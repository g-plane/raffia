# Raffia

Raffia is a parser which can parse CSS, SCSS, Sass (indented syntax) and Less. However, it won't compile SCSS, Sass or Less to CSS.

## 🧪 Playground

There is an online playground for inspecting AST. Visit: [https://raffia-play.vercel.app/](https://raffia-play.vercel.app/).

## 🍭 Example

```rust
use raffia::{ast::Stylesheet, Parser, Syntax};

let mut parser = Parser::new("a { color: green }", Syntax::Css);
let ast = parser.parse::<Stylesheet>().unwrap();
println!("{:#?}", ast);
```

You can find more examples in the [examples](https://github.com/g-plane/raffia/blob/main/raffia/examples) directory.

For detailed usage, check out [docs.rs](https://docs.rs/raffia).

## 🏗️ Current Status

-   CSS: Almost completed.
-   SCSS/Sass: Partially implemented. Some features are still in progress.
-   Less: Only a few features are implemented.

## ⌛ Benchmark

You can run compare performance with other parsers with benchmark.

First, you need to setup Rust and clone this repository. You also need to install `cargo-criterion` by running `cargo install cargo-criterion`.

Then, copy some CSS files to `bench_data` directory. You need to create that directory by yourself.

Now you can run benchmark by running `cargo criterion`.

## ✨ Credit

Tests come from:

-   [Web Platform Tests](https://github.com/web-platform-tests/wpt)
-   [SWC CSS parser](https://github.com/swc-project/swc/tree/main/crates/swc_css_parser/tests)
-   [ESBuild](https://github.com/evanw/esbuild/blob/master/internal/css_parser/css_parser_test.go)

## 📜 License

MIT License

Copyright (c) 2022-present Pig Fang
