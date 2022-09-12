use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use raffia::{ast::Stylesheet, Parser, Syntax};
use std::{env, fs, path::Path};

fn main() {
    let arg = env::args().skip(1).next().unwrap();
    let path = Path::new(&arg);
    let code = fs::read_to_string(&path).unwrap();

    let syntax = match path.extension().and_then(|ext| ext.to_str()) {
        Some("scss") => Syntax::Scss,
        Some("sass") => Syntax::Sass,
        Some("less") => Syntax::Less,
        _ => Syntax::Css,
    };

    let mut parser = Parser::new(&code, syntax);
    let result = parser.parse::<Stylesheet>();
    match result {
        Ok(ast) => println!("{:#?}", ast.span),
        Err(error) => {
            let file = SimpleFile::new(path.file_name().unwrap().to_str().unwrap(), &code);
            let diagnostic = Diagnostic::error()
                .with_message(error.kind.to_string())
                .with_labels(vec![Label::primary((), error.span.start..error.span.end)]);
            let writer = StandardStream::stderr(ColorChoice::Always);
            let config = term::Config::default();
            term::emit(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
        }
    }
}
