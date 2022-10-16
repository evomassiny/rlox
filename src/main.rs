use clap::Parser as ArgParser;

mod compiler;
mod lexer;

use crate::compiler::Compiler;

/// Command line arguments
#[derive(ArgParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to a lox file
    #[clap(short, long, value_parser)]
    input: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // build a scanner
    let _lexer = lexer::Lexer::from_path(&args.input)?;

    let lexer = lexer::Lexer::from_path(&args.input)?;
    let mut compiler = Compiler::new(Box::new(lexer));

    let obj_fn = compiler.compile();
    println!("obj_fn: {:?}", obj_fn);

    Ok(())
}
