use std::{path::Path, process::exit};

use clap::Parser;

// To be highly extended in the future.

#[derive(Parser)]
struct Interface {
    /// path to source file
    path: std::path::PathBuf,
}

pub struct Input {
    pub name: String,
    pub contents: String,
}

fn read_file<P: AsRef<Path>>(path: P) -> String {
    match std::fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(error) => {
            eprintln!("error while reading the source file: {}", error);
            exit(1);
        }
    }
}

pub fn get_input() -> Input {
    let args = Interface::parse();
    let path = args.path;
    let contents = read_file(&path);
    Input {
        name: path.to_string_lossy().into_owned(),
        contents,
    }
}
