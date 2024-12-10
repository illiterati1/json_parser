use std::io::Read;
use std::path::PathBuf;
use std::process::exit;
use std::fs::File;
use anyhow::{Context, Result};

use clap::Parser;
use lexer::Lexer;
use parser::Parser as MyParser;

mod lexer;
mod parser;

fn main() -> Result<()> {
    let args = Args::parse();
    let mut file = File::open(&args.path)
        .with_context(|| format!("Failed to open '{}'", args.path.to_string_lossy()))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .with_context(|| format!("Failed to read '{}'", args.path.to_string_lossy()))?;
    let lexer = Lexer::new(&contents);
    let mut parser = MyParser::new(lexer);
    match parser.parse() {
        Ok(Some(tree)) => {
            println!("{tree:?}")
        }
        Ok(None) => {}
        Err(e) => {
            println!("Error parsing '{}': {e}", args.path.to_string_lossy());
            exit(1);
        }
    }
    Ok(())
}

// Simple JSON parser
#[derive(Parser, Debug)]
struct Args {
    // The input file path
    path: PathBuf,
}
