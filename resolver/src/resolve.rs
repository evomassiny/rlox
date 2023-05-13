use super::symbols::{SymbolId, Symbol, SymbolTable};
use parser::{Stmt, StmtKind, Expr, ExprKind};
use lexer::Span;

pub enum NameError { }

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

    let mut symbols = SymbolTable::new();

    // TODO:
    // Traverse the AST and maintains some kind lexical scope chain,
    // eg: a linked list of HashMap<String, SymbolId>
    //
    todo!()
}
