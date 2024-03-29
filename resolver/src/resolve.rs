use super::scopes::{Globals, ScopeChain};
use super::symbols::{Sym, Symbol, SymbolId, SymbolTable};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};

#[derive(Debug)]
pub enum NameError {
    /// happens when a symbol has been redined twice in the same scope.
    RedefinitionError(String, Span, Span),
    UnboundedVariable(String, Span),
}

#[derive(Debug)]
pub struct Ast {
    roots: Vec<Stmt<Sym>>,
    symbols: SymbolTable,
}

fn resolve_block_stmt<'table>(
    stmts: Vec<Stmt<String>>,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    chain.push_scope();
    let mut block_stmts: Vec<Stmt<Sym>> = Vec::new();
    for in_body_stmt in stmts {
        block_stmts.push(resolve_lexical_scope(in_body_stmt, chain)?);
    }
    chain.pop_scope();
    Ok(StmtKind::Block(block_stmts))
}

fn resolve_class_stmt<'table>(
    name: String,
    super_name: Option<String>,
    methods: Vec<Stmt<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    let name: Sym = chain.add(name, src.clone());
    let super_name = match super_name {
        Some(super_name) => match chain.resolve(&super_name) {
            Some(super_name) => Some(super_name),
            None => return Err(NameError::UnboundedVariable(super_name, src.clone())),
        },
        None => None,
    };

    let mut out_methods: Vec<Stmt<Sym>> = Vec::new();
    for method in methods {
        out_methods.push(resolve_lexical_scope(method, chain)?);
    }
    chain.pop_scope();
    Ok(StmtKind::Class(name, super_name, out_methods))
}

/// recursively traverse the AST starting from `in_stmt`,
/// and resolve variable names along the way.
fn resolve_lexical_scope<'table>(
    in_stmt: Stmt<String>,
    chain: &mut ScopeChain<'table>,
) -> Result<Stmt<Sym>, NameError> {
    use StmtKind::*;
    let out_kind: StmtKind<Sym> = match in_stmt.kind {
        Block(stmts) => resolve_block_stmt(stmts, chain)?,
        Class(name, maybe_super_name, methods) => {
            resolve_class_stmt(name, maybe_super_name, methods, &in_stmt.span, chain)?
        }
        If(condition, then, maybe_else) => todo!(),
        Function(name, args, body) => todo!(),
        Expr(expr) => todo!(),
        Print(expr) => todo!(),
        Return(maybe_expr) => todo!(),
        Var(name, intializer) => todo!(),
        While(condition, body) => todo!(),
        For(maybe_initializer, maybe_condition, maybe_increment, body) => todo!(),
    };
    Ok(Stmt {
        kind: out_kind,
        span: in_stmt.span,
    })
}

/// Resolve all globals from the top ast node
fn resolve_globals(in_ast: &Vec<Stmt<String>>, symbols: &mut SymbolTable) -> Globals {
    let mut globals = Globals::new();
    use StmtKind::*;
    for stmt in in_ast {
        match stmt.kind {
            Class(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), symbols);
            }
            Function(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), symbols);
            }
            Var(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), symbols);
            }
            _ => continue,
        }
    }
    globals
}

/// traverse the input Abstract Syntax Tree,
/// and do name resolution.
///
/// In the process, create a symbol table, and use references to its content
/// in the output AST
pub fn resolve_names(in_ast: Vec<Stmt<String>>) -> Result<Ast, NameError> {
    // Traverse the AST in 2 passes:
    // * do a quick shallow pass, to locate globals
    // * do a real traversal, resolving everything

    let mut symbols = SymbolTable::new();

    // 1. resolve globals
    let globals = resolve_globals(&in_ast, &mut symbols);
    dbg!(&globals.symbols);

    // 2. perform a pre-order tree traversal, and maintains some kind of lexical scope chain,
    let mut chain = ScopeChain::new(globals, &mut symbols);
    let mut out_stmts: Vec<Stmt<Sym>> = Vec::new();
    for stmt in in_ast {
        let stmt = resolve_lexical_scope(stmt, &mut chain)?;
        out_stmts.push(stmt);
    }

    todo!()
}
