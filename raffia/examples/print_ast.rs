use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use raffia::{Parser, Syntax, ast::Stylesheet, error::Error};
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
        Ok(ast) => {
            println!("{:#?}", ast);
            let recoverable_errors = parser.recoverable_errors();
            if !recoverable_errors.is_empty() {
                println!("========");
                recoverable_errors
                    .iter()
                    .for_each(|error| print_error(path, &code, error));
            }
        }
        Err(error) => print_error(path, &code, &error),
    }
}

fn print_error(path: &Path, code: &str, error: &Error) {
    let file = SimpleFile::new(path.file_name().unwrap().to_str().unwrap(), &code);
    let diagnostic = Diagnostic::error()
        .with_message(error.kind.to_string())
        .with_labels(vec![Label::primary((), error.span.start..error.span.end)]);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = term::Config::default();
    term::emit_to_write_style(&mut writer.lock(), &config, &file, &diagnostic).unwrap();
}
