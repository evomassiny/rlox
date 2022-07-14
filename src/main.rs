use std::fs::File;
use std::io::prelude::*;

use clap::Parser;

mod lexer;
mod reader;
mod buffer;


/// Simple program to greet a person
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to a lox file
    #[clap(short, long, value_parser)]
    input: String,
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    

    /*
     * TODO!
     * implement Lexer::from_str() to fix tests.
     * Using some kind of string reader
     */
    // build a scanner
    let mut lexer = lexer::Lexer::from_path(&args.input)?;

    'scan_loop: loop {
        match lexer.scan_next() {
            Ok(token) => {
                println!("{:?}", &token);
                if token.kind == lexer::TokenKind::Eof {
                    break 'scan_loop;
                }
             },
            Err(e) => {
                println!("{:?}", e);
                break 'scan_loop;
            }
        }
    }
        
    
    Ok(())
}
