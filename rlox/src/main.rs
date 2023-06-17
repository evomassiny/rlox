use clap::Parser as ArgParser;
use lexer::{TokenKind, Tokenize};
use resolver::{resolve_names, Ast};

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
    let mut lexer = lexer::Lexer::from_path(&args.input)?;
    while let Ok(t) = lexer.scan_next() {
        println!("token: {:?}", &t);
        if matches!(t.kind, TokenKind::Eof) {
            break;
        };
    }

    let lexer = lexer::Lexer::from_path(&args.input)?;
    let mut parser = parser::StmtParser::new(Box::new(lexer));

    let raw_stmts = parser.parse();
    println!("raw_stmts: {:?}", raw_stmts);

    let raw_ast = raw_stmts
        .expect("parsing failed.")
        .pop()
        .expect("parsing failed.");
    let ast = resolve_names(raw_ast);
    println!("ast: {:?}", ast);

    Ok(())
}
