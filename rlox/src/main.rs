use clap::Parser as ArgParser;
use lexer::{Lexer, TokenKind, Tokenize};
use parser::{ParseError, StmtParser};
use resolver::{resolve_names, NameError};
use type_checker::type_check;

/// Command line arguments
#[derive(ArgParser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    /// Path to a lox file
    #[clap(short, long, value_parser)]
    input: String,

    /// print the tokens
    #[clap(long, value_parser)]
    print_tokens: bool,

    /// print AST, before name resolution
    #[clap(long, value_parser)]
    print_raw_ast: bool,
}

/// parse and print tokens.
fn print_tokens(src_path: &str) -> Result<(), Box<dyn std::error::Error>> {
    // build a scanner
    let mut lexer = Lexer::from_path(src_path)?;
    while let Ok(t) = lexer.scan_next() {
        println!("{:?}", &t);
        if matches!(t.kind, TokenKind::Eof) {
            break;
        };
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    if args.print_tokens {
        print_tokens(&args.input)?;
    }

    // 1- lexing
    let lexer = Lexer::from_path(&args.input)?;

    // 2- parsing
    // TODO:
    // return span location,
    // and print it the source context.
    let mut parser = StmtParser::new(Box::new(lexer));
    let raw_stmts = match parser.parse() {
        Ok(stmts) => stmts,
        Err(error) => match error {
            ParseError::ExpectedToken(msg) => panic!("Missing token {:?}", msg),
            ParseError::ScanningError(e) => panic!("Lexing error {:?}", e),
            ParseError::ExpectedExpression(msg) => {
                panic!("missing expression: {:?}", msg)
            }
            ParseError::Starved => panic!("File ended too soon !"),
        },
    };
    if args.print_raw_ast {
        for stmt in &raw_stmts {
            println!("{:?}", &stmt);
        }
    }

    // 3 - name resolution
    // TODO: print a proper error message
    let ast = match resolve_names(raw_stmts) {
        Ok(ast) => ast,
        Err(NameError::RedefinitionError(name, src_a, src_b)) => panic!(
            "'{name}' defined twice, at l.{0} and l.{1}",
            src_a.line, src_b.line
        ),
        Err(NameError::UnboundedVariable(name, src)) => {
            panic!("undefined variable '{name}', at l.{0}", src.line)
        }
    };
    println!("ast: {:?}\n", ast);
    println!("{}", &ast.symbols);

    // 4 - type checking
    let _ = type_check(ast);

    Ok(())
}
