use std::path::Path;

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

fn read_file<P: AsRef<Path>>(path: P) -> std::io::Result<String> {
    std::fs::read_to_string(path)
}

pub fn get_input() -> std::io::Result<Input> {
    let args = Interface::parse();
    let path = std::fs::canonicalize(args.path)?;
    let contents = read_file(&path)?;
    Ok(Input {
        name: path.to_string_lossy().into_owned(),
        contents,
    })
}
