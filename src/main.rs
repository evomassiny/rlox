use std::fs::File;
use std::io::prelude::*;

use clap::Parser;

mod lexer;

use crate::lexer::{Lexer, TokenKind};

/// Command line arguments
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to a lox file
    #[clap(short, long, value_parser)]
    input: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // build a scanner
    let mut lexer = lexer::Lexer::from_path(&args.input)?;

    'scan_loop: loop {
        match lexer.scan_next() {
            Ok(token) => {
                println!("{:?}", &token);
                if token.kind == TokenKind::Eof {
                    break 'scan_loop;
                }
            }
            Err(e) => {
                println!("{:?}", e);
                break 'scan_loop;
            }
        }
    }

    Ok(())
}
