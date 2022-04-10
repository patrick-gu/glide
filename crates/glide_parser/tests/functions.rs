use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn generic_func() {
    let source = Source::anonymous(
        r#"
func foo<A, B, C>(a A, b B, c C, foo Foo<A, B, C,>,) Foo<A, B, C> {
    foo(a, b, c)
}

func bar<T>(foo Bar<Bar<Bar<Bar<T>, T>, T,>, Bar<T>>) {}
"#,
    );
    let ast = parse(&source);
    panic!("{:#?}", ast);
}
