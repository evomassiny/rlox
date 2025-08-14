/// Performs name resolution on a raw AST,
/// in order to build another AST with its name resolved _and_ a symbol table.
mod resolve;
mod scopes;
mod symbols;

pub use resolve::{resolve_names, Ast, NameError};
pub use symbols::{StorageKind, Symbol, SymbolId, SymbolTable};

#[cfg(test)]
mod resolver {
    use super::{resolve_names, StorageKind};
    use lexer::{Lexer, StrPeeker};
    use parser::{Stmt, StmtParser};

    fn parse(src: &'static str) -> Vec<Stmt<String>> {
        let lexer: Lexer<StrPeeker<'_, 64>> = Lexer::from_str(src);
        let mut parser = StmtParser::new(Box::new(lexer));
        parser
            .parse()
            .expect("these test examples sources must be parseable !")
    }

    #[test]
    /// test that shadowing a variable in a single scope
    /// is disallowed
    fn check_var_declaration_duplication() {
        let src = r#"
        fun foo() {
          var a = 1;
          var a = 1;
        }
        "#;
        let ast = parse(src);
        assert!(resolve_names(ast).is_err());
    }

    #[test]
    /// assert that shadowing a variable across scopes is allowed.
    fn check_var_shadowing() {
        let src = r#"
        var a = 1;

        fun foo() {
          var a = 1;
        }
        foo();
        "#;
        let ast = parse(src);
        assert!(resolve_names(ast).is_ok());
    }

    #[test]
    /// test that using an undefined variable raises an error.
    fn check_undefined_variable() {
        let src = r#"
        fun foo() {
          a = 1;
        }
        "#;
        let ast = parse(src);
        assert!(resolve_names(ast).is_err());
    }

    #[test]
    /// assert that upvalues are promoted as such
    fn check_upvalue_promotion() {
        let src = r#"
        fun foo() {
          var a = 1;
          fun closure() {
              a = a + 1;
              return a;
          }
          return closure;
        }
        "#;
        let ast = parse(src);
        let ast = resolve_names(ast).expect("failed to resolve valid code.");
        let symbol_for_a = ast
            .symbols
            .symbols
            .iter()
            .find(|symbol| symbol.name == "a")
            .expect("symbol 'a' not found.");
        assert_eq!(symbol_for_a.storage_kind, StorageKind::UpValue);
    }
}
