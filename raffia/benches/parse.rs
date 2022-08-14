use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::fs;

fn parse_with_raffia(code: &str) -> raffia::ast::Stylesheet {
    let mut parser = raffia::Parser::new(code, raffia::Syntax::Css);
    parser.parse().unwrap()
}

fn parse_with_parcel(code: &str) -> parcel_css::stylesheet::StyleSheet {
    parcel_css::stylesheet::StyleSheet::parse(
        code,
        parcel_css::stylesheet::ParserOptions {
            filename: "test.css".into(),
            nesting: true,
            custom_media: true,
            ..Default::default()
        },
    )
    .unwrap()
}

fn parse_with_swc(source_file: &swc_common::SourceFile) -> swc_css_ast::Stylesheet {
    swc_css_parser::parse_file(source_file, Default::default(), &mut Default::default()).unwrap()
}

fn create_swc_source_file(code: &str) -> swc_common::SourceFile {
    use swc_common::{BytePos, FileName, SourceFile};
    SourceFile::new(
        FileName::Custom("test.css".into()),
        false,
        FileName::Custom("test.css".into()),
        code.into(),
        BytePos(1),
    )
}

fn bench_parsers(c: &mut Criterion) {
    let code = fs::read_to_string("test.css").unwrap();
    let swc_source_file = create_swc_source_file(&code);

    let mut group = c.benchmark_group("Parser");
    group.bench_with_input(BenchmarkId::new("Raffia", "test.css"), &code, |b, code| {
        b.iter(|| parse_with_raffia(code))
    });
    group.bench_with_input(BenchmarkId::new("Parcel", "test.css"), &code, |b, code| {
        b.iter(|| parse_with_parcel(code))
    });
    group.bench_with_input(
        BenchmarkId::new("SWC", "test.css"),
        &swc_source_file,
        |b, source_file| b.iter(|| parse_with_swc(source_file)),
    );
    group.finish();
}

criterion_group!(benches, bench_parsers);
criterion_main!(benches);
