use glide_compiler::compile;
use glide_parser::parse;
use glide_span::source::Source;

#[test]
fn fibonacci() {
    let source = Source::anonymous(include_str!("../../../examples/fibonacci.gl"));
    let ast = parse(&source).unwrap();
    let ir = compile(&ast).unwrap();
    dbg!(&ir);
}
