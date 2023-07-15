use super::symbols::{Sym, SymbolId, SymbolTable};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};
use std::collections::HashMap;

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
        Self {
            symbols: HashMap::new(),
        }
    }

    /// register a global (alongside its span)
    pub fn add(&mut self, name: String, src: Span, table: &mut SymbolTable) -> SymbolId {
        let symbol_id = table.add(name.clone(), src);

        self.symbols
            .entry(name)
            .or_insert_with(Vec::<SymbolId>::new)
            .push(symbol_id);
        symbol_id
    }

    /// lookup for the last global variable
    /// named `name` defined before `src`.
    pub fn resolve_precise(&self, name: &str, src: &Span, table: &SymbolTable) -> Option<SymbolId> {
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

    pub fn resolve(&self, name: &str, table: &SymbolTable) -> Option<Sym> {
        let mut symbs: Vec<SymbolId> = Vec::new();
        if let Some(globals) = self.symbols.get(name) {
            if globals.len() == 0 {
                return Some(Sym::Direct(globals[0]));
            }
            return Some(Sym::OneOf(globals.clone().into_boxed_slice()));
        }
        None
    }
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
        Self {
            symbols: HashMap::new(),
        }
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

pub struct ScopeChain<'table> {
    symbols: &'table mut SymbolTable,
    globals: Globals,
    chain: Vec<Scope>,
}
impl<'table> ScopeChain<'table> {
    pub fn new(globals: Globals, symbols: &'table mut SymbolTable) -> Self {
        Self {
            globals,
            symbols,
            chain: Vec::new(),
        }
    }

    pub fn push_scope(&mut self) {
        self.chain.push(Scope::new());
    }

    pub fn pop_scope(&mut self) {
        let _ = self.chain.pop();
    }

    pub fn add(&mut self, name: String, src: Span) -> Sym {
        let symbol_id = match self.chain.last_mut() {
            Some(scope) => scope.add(name, src, self.symbols),
            None => unreachable!("Can't append a "),
        };
        Sym::Direct(symbol_id)
    }

    pub fn resolve(&self, name: &str) -> Option<Sym> {
        // first lookup in the lexical scope chain
        if self.chain.len() > 0 {
            let mut scope_idx = self.chain.len() - 1;
            while scope_idx >= 0 {
                if let Some(id) = self.chain[scope_idx].resolve(name) {
                    return Some(Sym::Direct(id));
                }
                scope_idx -= 1;
            }
        }
        // fallback to globals
        self.globals.resolve(name, self.symbols)
    }
}
