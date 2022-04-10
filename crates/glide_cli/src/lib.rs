use std::{env, fs};

pub fn run() {
    let args: Vec<String> = env::args().collect();
    match &args[..] {
        [_, file] => {
            let data = fs::read_to_string(file).unwrap();
            glide::run_named(file.to_owned(), data);
        }
        [..] => {
            eprintln!("usage: glide_cli <file.gl>");
        }
    }
}
