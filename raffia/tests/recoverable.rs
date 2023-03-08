use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFile,
    term::{self, termcolor::Buffer},
};
use insta::{assert_snapshot, glob, Settings};
use raffia::{ast::Stylesheet, Parser, Syntax};
use std::fs;

#[test]
fn recoverable_errors_snapshot() {
    glob!("recoverable/**/*.{css,scss,sass,less}", |path| {
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

        let file = SimpleFile::new(file_name, &code);
        let config = term::Config::default();

        let errors = match parser.parse::<Stylesheet>() {
            Ok(..) => {
                let recoverable_errors = parser.recoverable_errors();
                assert!(
                    !recoverable_errors.is_empty(),
                    "'{}' should contain recoverable errors",
                    path.display()
                );

                let mut buffer = Buffer::no_color();
                recoverable_errors
                    .iter()
                    .map(|error| {
                        Diagnostic::error()
                            .with_message(error.kind.to_string())
                            .with_labels(vec![Label::primary((), error.span.start..error.span.end)])
                    })
                    .for_each(|diagnostic| {
                        term::emit(&mut buffer, &config, &file, &diagnostic).unwrap();
                    });
                String::from_utf8(buffer.into_inner()).unwrap()
            }
            Err(error) => {
                let diagnostic = Diagnostic::error()
                    .with_message(error.kind.to_string())
                    .with_labels(vec![Label::primary((), error.span.start..error.span.end)]);
                let mut buffer = Buffer::ansi();
                term::emit(&mut buffer, &config, &file, &diagnostic).unwrap();
                panic!("\n{}", String::from_utf8(buffer.into_inner()).unwrap());
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
            assert_snapshot!(file_name, errors);
        });
    });
}
