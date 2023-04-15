use clap::Parser as ArgParser;




use lexer::{TokenKind, Tokenize};

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

    let ast = parser.parse();
    println!("ast: {:?}", ast);

    Ok(())
}
