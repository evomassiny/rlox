use super::symbols::{Symbol, SymbolId, SymbolTable};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};

#[derive(Debug)]
pub enum NameError {}

#[derive(Debug)]
pub struct Ast {
    root: Stmt<SymbolId>,
    symbols: SymbolTable,
}

/// traverse the input Abstract Syntax Tree,
/// an do name resolution.
///
/// In the process, create a symbol table, and use references to its content
/// in the output AST
pub fn resolve_names(in_ast: Stmt<String>) -> Result<Ast, NameError> {
    // Traverse the AST in 2 passes:
    // * do a quick shallow pass, to locate globals
    // * do a real traversal, resolving everything

    let mut globals = SymbolTable::new();
    // Resolution of globals is trickier than it seems,
    // consider the following:
    // ```lox
    // fun foo() { bar(); }
    // fun bar() { print "first"; }
    // foo();
    // fun bar() { print "second"; }
    // foo();
    // ```
    // the 2 invokations of `foo()` won't lead to the same
    // `bar()` call.
    // Said otherwise, the resoltion of a global variable depends of
    // the callsite.
    // This is why we need to store them sepaately.
    dbg!(in_ast);

    // TODO:
    // Traverse the AST and maintains some kind lexical scope chain,
    // eg: a linked list of HashMap<String, SymbolId>
    todo!()
}
