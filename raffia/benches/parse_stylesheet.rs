use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use std::{fs, time::Duration};

fn parse_with_raffia(code: &str) -> raffia::ast::Stylesheet {
    let mut parser = raffia::Parser::new(code, raffia::Syntax::Css);
    parser.parse().unwrap()
}

fn parse_with_parcel<'a>(
    code: &'a str,
    filename: &str,
) -> parcel_css::stylesheet::StyleSheet<'a, 'a> {
    parcel_css::stylesheet::StyleSheet::parse(
        code,
        parcel_css::stylesheet::ParserOptions {
            filename: filename.into(),
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

fn create_swc_source_file(code: &str, filename: &str) -> swc_common::SourceFile {
    use swc_common::{BytePos, FileName, SourceFile};
    SourceFile::new(
        FileName::Custom(filename.into()),
        false,
        FileName::Custom(filename.into()),
        code.into(),
        BytePos(1),
    )
}

fn bench_parsers(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_stylesheet");
    group.measurement_time(Duration::from_secs(12));

    fs::read_dir(".")
        .unwrap()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            let name = entry.file_name();
            let name = name.to_string_lossy();
            name.starts_with("bench.") && name.ends_with(".css")
        })
        .for_each(|entry| {
            let path = &entry.path();
            let name = entry.file_name();
            let name = &name.to_string_lossy();
            let code = fs::read_to_string(path).unwrap();
            let swc_source_file = create_swc_source_file(&code, &name);

            group.bench_with_input(BenchmarkId::new("Raffia", name), &code, |b, code| {
                b.iter(|| parse_with_raffia(code))
            });
            group.bench_with_input(BenchmarkId::new("Parcel", name), &code, |b, code| {
                b.iter(|| parse_with_parcel(code, &name))
            });
            group.bench_with_input(
                BenchmarkId::new("SWC", name),
                &swc_source_file,
                |b, source_file| b.iter(|| parse_with_swc(source_file)),
            );
        });
    group.finish();
}

criterion_group!(benches, bench_parsers);
criterion_main!(benches);
