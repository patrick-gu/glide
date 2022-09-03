#[cfg(not(target_pointer_width = "64"))]
compile_error!("only works on 64 bit systems");

use std::{
    fs,
    path::{Path, PathBuf},
};

use glide_codegen::{codegen, CodegenOpts};
use glide_compiler::{Compilation, StandardAsts};
use glide_parser::parse;
use glide_span::source::Source;

fn compilation() -> Compilation {
    let core = include_str!("../../../lib/core.gl");
    let core_source = Source {
        name: "core.gl".to_owned(),
        data: core.to_owned(),
    };
    let core_ast = parse(&core_source).unwrap();
    let prelude = include_str!("../../../lib/prelude.gl");
    let prelude_source = Source {
        name: "prelude.gl".to_owned(),
        data: prelude.to_owned(),
    };
    let prelude_ast = parse(&prelude_source).unwrap();
    let runtime = include_str!("../../../lib/runtime.gl");
    let runtime_source = Source {
        name: "runtime.gl".to_owned(),
        data: runtime.to_owned(),
    };
    let runtime_ast = parse(&runtime_source).unwrap();
    let standard_asts = StandardAsts {
        core: core_ast,
        prelude: prelude_ast,
        runtime: runtime_ast,
    };

    Compilation::new(&standard_asts)
}

#[derive(Debug)]
pub struct CompileOptions {
    pub input: PathBuf,
    pub output: PathBuf,
    // opt_level: OptLevel,
}

// #[derive(Copy, Clone, Debug)]
// pub enum OptLevel {
//     O0,
//     O1,
//     O2,
//     O3,
// }

pub fn compile(opts: &CompileOptions) {
    let data = fs::read_to_string(&opts.input).expect("failed to read source file");

    let mut compilation = compilation();

    let source = Source {
        name: opts.input.to_string_lossy().into_owned(),
        data,
    };

    let package = parse(&source).unwrap();

    let ir = compilation
        .compile_binary("binary".to_owned(), &package)
        .unwrap();

    codegen(CodegenOpts {
        ir: &ir,
        target: glide_codegen::TargetTriple::X86_64UnknownLinuxGnu,
        output: &opts.output,
    });
}
