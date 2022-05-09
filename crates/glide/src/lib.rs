#[cfg(not(target_pointer_width = "64"))]
compile_error!("only works on 64 bit systems");

use glide_span::source::Source;

pub fn run(string: String) {
    let source = glide_span::source::Source::anonymous(string);
    run_source(source);
}

pub fn run_named(name: String, data: String) {
    let source = glide_span::source::Source { name, data };
    run_source(source);
}

fn run_source(source: Source) {
    let ast = glide_parser::parse(&source).unwrap();
    let ir = glide_compiler::compile(&ast).unwrap();
    dbg!(&ir);
    glide_codegen::codegen(&ir);
}
