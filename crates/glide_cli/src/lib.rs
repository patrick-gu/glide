use std::{
    env, fs,
    path::{Path, PathBuf},
};

use clap::Parser;
use glide::{compile, CompileOptions};

/// Glide command line interface
#[derive(Parser, Debug)]
#[clap(name = "Glide", version)]
enum Args {
    Compile(Compile),
}

/// Compile a Glide program
#[derive(Parser, Debug)]
#[clap(name = "Glide compiler")]
struct Compile {
    /// Input file path
    #[clap(value_parser)]
    input: PathBuf,

    /// Output binary path
    #[clap(value_parser, short)]
    output: Option<PathBuf>,
}

pub fn run() {
    let args = Args::parse();
    match args {
        Args::Compile(Compile { input, output }) => {
            let output = output.unwrap_or_else(|| {
                let mut path = input.clone();
                path.set_extension("o");
                path
            });
            compile(&CompileOptions { input, output });
        }
    }
}
