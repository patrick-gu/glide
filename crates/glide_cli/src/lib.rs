use std::{env, fs, path::Path};

pub fn run() {
    let args: Vec<String> = env::args().collect();
    match &args[..] {
        [_, file] => {
            glide::compile(Path::new(file));
        }
        [..] => {
            eprintln!("usage: glide_cli <file.gl>");
        }
    }
}
