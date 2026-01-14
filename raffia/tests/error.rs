use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term,
};
use insta::{assert_snapshot, glob, Settings};
use raffia::{ast::Stylesheet, Parser, Syntax};
use std::fs;

#[test]
fn error_snapshot() {
    glob!("error/**/*.{css,scss,sass,less}", |path| {
        let file_name = path.file_name().unwrap().to_str().unwrap();

        let code = fs::read_to_string(path).unwrap();
        let syntax = match path.extension().unwrap().to_str().unwrap() {
            "css" => Syntax::Css,
            "scss" => Syntax::Scss,
            "sass" => Syntax::Sass,
            "less" => Syntax::Less,
            _ => unreachable!("unknown file extension"),
        };
        let mut parser = Parser::new(&code, syntax);
        let error = match parser.parse::<Stylesheet>() {
            Ok(..) => panic!(
                "\"{}\" should contain unrecoverable syntax error, but actually parsed successfully.",
                path.display()
            ),
            Err(error) => {
                let file = SimpleFile::new(file_name, &code);
                let diagnostic = Diagnostic::error()
                    .with_message(error.kind.to_string())
                    .with_labels(vec![Label::primary((), error.span.start..error.span.end)]);
                let config = term::Config::default();
                term::emit_into_string(&config, &file, &diagnostic).unwrap()
            }
        };

        let mut settings = Settings::clone_current();
        settings.set_snapshot_path(path.parent().unwrap());
        settings.remove_snapshot_suffix();
        settings.set_prepend_module_to_snapshot(false);
        settings.remove_input_file();
        settings.set_omit_expression(true);
        settings.remove_input_file();
        settings.remove_info();
        settings.bind(|| {
            let name = path.file_stem().unwrap().to_str().unwrap();
            assert_snapshot!(name, error);
        });
    });
}
