use std::io::Read;
use std::path::PathBuf;
use std::process::exit;
use std::fs::File;
use anyhow::Result;

use clap::Parser;
use lexer::Lexer;
use parser::Parser as MyParser;

mod lexer;
mod parser;

fn main() -> Result<()> {
    let args = Args::parse();
    let mut file = match File::open(&args.path) {
        Ok(f) => f,
        Err(e) => { 
            println!("Encountered error opening file {}: {e}", args.path.to_str().unwrap());
            exit(10);
        }
    };
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => {}
        Err(e) => {
            println!("Encountered error reading file {}: {e}", args.path.to_str().unwrap());
            exit(11);
        }
    }

    let mut lexer = Lexer::new(&contents);
    let tokens = lexer.lex()?;
    let parser = MyParser::new();
    match parser.parse(&tokens) {
        Ok(Some(tree)) => {
            println!("{tree:?}")
        }
        Ok(None) => {}
        Err(e) => {
            println!("Error parsing {}: {e}", args.path.to_str().unwrap());
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
