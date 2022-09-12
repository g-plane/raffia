use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

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
    let selector = "*|a-bc--xx#q09h6nn.--oq323r23r > |x76j5 + [mmmmm][oew=a i][\\0065='lll'], u6\\z:hover + m::after {}";
    let swc_source_file = create_swc_source_file(selector);

    let mut group = c.benchmark_group("parse_selector");

    group.bench_with_input(BenchmarkId::new("Raffia", selector), selector, |b, code| {
        b.iter(|| parse_with_raffia(code))
    });
    /* group.bench_with_input(BenchmarkId::new("Parcel", selector), selector, |b, code| {
        b.iter(|| parse_with_parcel(code))
    });
    group.bench_with_input(
        BenchmarkId::new("SWC", selector),
        &swc_source_file,
        |b, source_file| b.iter(|| parse_with_swc(source_file)),
    ); */
    group.finish();
}

criterion_group!(benches, bench_parsers);
criterion_main!(benches);
