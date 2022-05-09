use glide_compiler::{compile, error::Error};
use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn int_return_type_no_exprs() {
    let source = Source::anonymous(
        r#"
func foo() Int {
}
"#,
    );
    let ast = parse(&source).unwrap();
    let error = compile(&ast).unwrap_err();
    assert!(matches!(error, Error::TyMismatch));
}

#[test]
fn explicit_void_return() {
    let source = Source::anonymous(
        r#"
func foo() Void {
}

func main() {}
"#,
    );
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
}

#[test]
fn discard_value() {
    let source = Source::anonymous(
        r#"
func foo() {
    123
    321
    12321
}

func main() {}
"#,
    );
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
}

#[test]
fn return_int() {
    let source = Source::anonymous(
        r#"
func foo() Int {
    123
    321
    12321
}

func main() {}
"#,
    );
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
}

#[test]
fn recursion() {
    let source = Source::anonymous(
        r#"
func foo(a Slice<Int>) Slice<Int> {
    foo(a)
}

func main() {}
"#,
    );
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
}

#[test]
fn generics() {
    let source = Source::anonymous(
        r#"
func foo<T>(a Slice<T>) Slice<T> {
    a
}

func bar(b Slice<Int>) {
    foo(b)
}

func main() {
}
"#,
    );
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
}

#[test]
fn generics_2() {
    let source = Source::anonymous(
        r#"
func identity<T>(t T) T {
    t
}

func main() {
    identity(0)
    identity("0")
}
"#,
    );
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
    dbg!(&ir);
}

#[test]
fn main_has_generics() {
    let source = Source::anonymous(
        r#"
func main<T>() {}
"#,
    );
    let ast = parse(&source).unwrap();
    let error = compile(&ast).unwrap_err();
    assert!(matches!(error, Error::NoMain));
}
