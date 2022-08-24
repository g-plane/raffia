use insta::{assert_ron_snapshot, glob, Settings};
use raffia::{ast::Stylesheet, Parser, Syntax};
use std::fs;

#[test]
fn ast_snapshot() {
    glob!("ast/**/*.{css,scss,sass,less}", |path| {
        let code = fs::read_to_string(path).unwrap();
        let syntax = match path.extension().unwrap().to_str().unwrap() {
            "css" => Syntax::Css,
            "scss" => Syntax::Scss,
            "sass" => Syntax::Sass,
            "less" => Syntax::Less,
            _ => unreachable!("unknown file extension"),
        };
        let mut parser = Parser::new(&code, syntax);
        let ast = parser.parse::<Stylesheet>().unwrap();

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
            assert_ron_snapshot!(name, ast);
        });
    });
}
