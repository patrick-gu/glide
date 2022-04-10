use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn hello_world() {
    let source = Source::anonymous(
        r#"
func main() {
    print("Hello World!")
}
"#,
    );
    let ast = parse(&source).unwrap();
    panic!("{:#?}", ast);
}

#[test]
fn escape_quotation_backslash() {
    let source = Source::anonymous(
        r#"
func main() {
    "aaa\"aa\"\"\\b"
}
"#,
    );
    let ast = parse(&source).unwrap();
    panic!("{:#?}", ast);
}
