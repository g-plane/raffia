use raffia::{Parser, Syntax, ast::Stylesheet};
use std::{env, fs, path::Path};

fn main() {
    let arg = env::args().skip(1).next().unwrap();
    let path = Path::new(&arg);
    let file = fs::read_to_string(&path).unwrap();

    let syntax = match path.extension().and_then(|ext| ext.to_str()) {
        Some("scss") => Syntax::Scss,
        Some("sass") => Syntax::Sass,
        Some("less") => Syntax::Less,
        _ => Syntax::Css,
    };
    let ast = Parser::new(&file, syntax).parse::<Stylesheet>().unwrap();
    let json = serde_json::to_string_pretty(&ast).unwrap();
    println!("{}", json);
}
