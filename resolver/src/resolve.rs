use super::symbols::{Symbol, SymbolId, SymbolTable};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};
use std::collections::HashMap;

#[derive(Debug)]
pub enum NameError {}

#[derive(Debug)]
pub struct Ast {
    root: Stmt<SymbolId>,
    symbols: SymbolTable,
}

pub struct Scope {
    // in a non-global scope,
    // the redefinition of a variable is not allowed,
    // so we don't need to store multiple variables
    // per name.
    symbols: HashMap<String, SymbolId>,
}
impl Scope {
    pub fn new() -> Self {
        Self { symbols: HashMap::new() }
    }

    pub fn add(&mut self, name: String, src: Span, table: &mut SymbolTable) -> SymbolId {
        let id = table.add(name.clone(), src);
        self.symbols.insert(name, id);
        id
    }
    pub fn resolve(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }
}

pub struct ScopeChain {
    chain: Vec<Scope>,
}
impl ScopeChain {
    pub fn new() -> Self {
        Self { chain: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.chain.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        let _ = self.chain.pop();
    }

    pub fn add(&mut self, name: String, src: Span, table: &mut SymbolTable) -> SymbolId {
        let symbol_id =  match self.chain.last_mut() {
            Some(scope) => scope.add(name, src, table),
            None => unreachable!("Can't append a "),
        };
        symbol_id
    }

    pub fn resolve(&self, name: &str) -> Option<SymbolId> {
        let mut scope_idx = self.chain.len() -1;
        while scope_idx >= 0 {

            if let Some(id) = self.chain[scope_idx].resolve(name) {
                return Some(id);
            }
            scope_idx -= 1;
        }
        None
    }

}

/// Resolution of globals is trickier than it seems,
/// consider the following:
/// ```lox
/// fun foo() { bar(); }
/// fun bar() { print "first"; }
/// foo();
/// fun bar() { print "second"; }
/// foo();
/// ```
/// the 2 invokations of `foo()` won't lead to the same
/// `bar()` call.
/// Said otherwise, the resolution of a global variable depends of
/// the callsite.
/// This is why we need to store them separetly.
pub struct Globals {
    /// redefinition is allowed in globals,
    pub symbols: HashMap<String, Vec<SymbolId>>,
}
impl Globals {
    pub fn new() -> Self {
        Self { symbols: HashMap::new() }
    }

    /// register a global (alongside its span)
    pub fn add(&mut self, name: String, src: Span, table: &mut SymbolTable) -> SymbolId {
        let symbol_id = table.add(name.clone(), src);

        self.symbols.entry(name)
            .or_insert_with(Vec::<SymbolId>::new)
            .push(symbol_id);
        symbol_id
    }

    /// lookup for the last global variable
    /// named `name` defined before `src`.
    pub fn resolve(&self, name: &str, src: &Span, table: &SymbolTable) -> Option<SymbolId> {
        let mut last: Option<SymbolId> = None;
        if let Some(globals) = self.symbols.get(name) {
            for global_id in globals {
                if table[global_id].src.line > src.line {
                    return last;
                }
                last = Some(*global_id);
            }
        }
        last
    }
}


fn resolve_names_in_stmt(in_stmts: Stmt<String>, scopes: ScopeChain) -> Result<Stmt<SymbolId>, NameError> {
    todo!()
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
    let mut globals = Globals::new();
    for stmt in &in_ast {
        match stmt.kind {
            StmtKind::Class(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), &mut symbols);
            }
            StmtKind::Function(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), &mut symbols);
            }
            StmtKind::Var(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), &mut symbols);
            }
            _ => continue
        }
    }
    dbg!(&globals.symbols);

    // TODO:
    // Traverse the AST and maintains some kind lexical scope chain,
    // eg: a linked list of HashMap<String, SymbolId>
    todo!()
}
