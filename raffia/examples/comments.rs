use raffia::{ParserBuilder, ast::Stylesheet};

fn main() {
    let mut comments = vec![];
    let mut parser = ParserBuilder::new(
        "
a {
    /* comment */
    color: green;
}
    ",
    )
    .comments(&mut comments)
    .build();
    let _ = parser.parse::<Stylesheet>().unwrap();
    println!("{:#?}", comments);
}
