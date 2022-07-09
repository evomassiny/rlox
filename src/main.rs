use std::fs::File;
use std::io::prelude::*;

use clap::Parser;

mod lexer;


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
    
    // read file
    // TODO: construct some kind of utf8 chars iterator,
    // so we can compile the script as we're reading it
    let mut file = File::open(&args.input)?;
    let mut src = String::new();
    let _ = file.read_to_string(&mut src)?;

    // build a scanner
    let mut lexer = lexer::Lexer::new(&src);

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
