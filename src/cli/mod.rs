use std::{fs::File, io::Read, process::exit};

use clap::Parser;

// To be highly extended in the future.

#[derive(Parser)]
struct Interface {
    /// path to source file
    path: std::path::PathBuf,
}

fn read_file(path: std::path::PathBuf) -> String {
    let file = File::open(path);
    match file {
        Ok(mut file) => {
            let mut content = String::new();
            let _ = file.read_to_string(&mut content);
            content
        }
        Err(error) => {
            println!("oopsie, error during reading of the source file: {}", error);
            exit(1);
        }
    }
}

pub fn get_input() -> String {
    let args = Interface::parse();
    let path = args.path;
    let content = read_file(path);
    content
}
