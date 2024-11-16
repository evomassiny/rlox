/// Performs name resolution on a raw AST,
/// in order to build another AST with its name resolved _and_ a symbol table.
mod resolve;
mod scopes;
mod symbols;

pub use resolve::{resolve_names, Ast, NameError};
use scopes::{Globals, ScopeChain};
pub use symbols::{Sym, Symbol, SymbolId, SymbolTable};

#[cfg(test)]
mod stmt_parsing {
    use parser::{Stmt,StmtParser};
    use lexer::{Lexer, StrPeeker};
    use super::{resolve_names, NameError,Symbol};

    fn parse(src: &'static str) -> Vec<Stmt<String>> {
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut parser = StmtParser::new(Box::new(lexer));
        parser.parse().expect("these test examples sources must be parseable !")
    }

    #[test]
    /// test parsing a single expression statement
    fn check_var_declaration_duplication() {
        let src = r#"
        fun foo() {
          var a = 1;
          var a = 1;
        }
        "#;
        let mut ast = parse(src);
        assert!(resolve_names(ast).is_err());
    }


}
