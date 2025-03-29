use super::symbols::{StorageKind, SymbolId, SymbolTable};
use lexer::Span;
use std::collections::HashMap;


/// When we traverse the AST, we end up
/// encountering 4 kinds of scope,
/// keeping track of them allow us to
/// distinguish captured variable from the others,
/// and resolve the type of `super` and `this`
#[derive(Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Block,
    FunDecl,
    ClassDecl,
    Global,
}

pub struct Scope {
    // in a non-global scope,
    // the redefinition of a variable is not allowed,
    // so we don't need to store multiple variables
    // per name.
    symbols: HashMap<String, SymbolId>,
    kind: ScopeKind,
}
impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            symbols: HashMap::new(),
            kind,
        }
    }

    pub fn add(
        &mut self,
        name: String,
        src: Span,
        table: &mut SymbolTable,
    ) -> SymbolId {
        let id = table.add(name.clone(), src);
        self.symbols.insert(name, id);
        id
    }
    pub fn resolve(&self, name: &str) -> Option<SymbolId> {
        self.symbols.get(name).copied()
    }
}

/// This struct holds everything we need to keep track
/// of all variable bindings when we traverse it,
/// right after resolving all the global variables.
///
/// To use it: everytime we encounter a new scope,
/// we need to register it (using `push_scope`) and pop it afterwards.
/// Use it to register new bindings, and
/// query a variable name to resolve its binding.
pub struct ScopeChain<'table> {
    // keeps track of all declared symbols
    symbols: &'table mut SymbolTable,
    // a stack which should mimick the nesting of scopes
    // we are traversing.
    chain: Vec<Scope>,
}
impl<'table> ScopeChain<'table> {
    pub fn new(symbols: &'table mut SymbolTable) -> Self {
        Self {
            symbols,
            chain: Vec::new(),
        }
    }

    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.chain.push(Scope::new(kind));
    }

    pub fn pop_scope(&mut self) {
        let _ = self.chain.pop();
    }

    pub fn add(&mut self, name: String, src: Span) -> SymbolId {
        let symbol_id = match self.chain.last_mut() {
            // inner scope
            Some(scope) => scope.add(name, src, self.symbols),
            // We could append the binding directly into the global scope,
            // _but_ it should have been previously resolved, in a dedicated pass
            None => unreachable!("the binding '{name}' should have already been resolved")
        };
        symbol_id
    }

    /// If `name` was already declared in the local scope,
    /// returns its definition location.
    pub fn location_of_declaration_in_current_scope(
        &self,
        name: &str,
    ) -> Option<Span> {
        if let Some(scope) = self.chain.last() {
            if let Some(symbol_id) = scope.resolve(name) {
                // symbol present in chain scope should always exist in the symbol table.
                let symbol = &self.symbols[symbol_id];
                return Some(symbol.src.clone());
            }
        }
        None
    }

    /// traverse the scope chain to find the nearest declaration of `name`
    /// In the process, promote variable as upvalues if needed.
    pub fn resolve(&mut self, name: &str) -> Option<SymbolId> {
        // first lookup in the lexical scope chain
        for scope_idx in (0..self.chain.len()).rev() {
            if let Some(symbol_id) = self.chain[scope_idx].resolve(name) {
                // search for a function declaration scope
                // between where the variable was declared,
                // and the cureent usage.
                // If we found one, this means we're dealing with an
                // upvalue, eg: a variable captured by a closure.
                if scope_idx != self.chain.len() - 1 {
                    for i in (scope_idx + 1)..self.chain.len() {
                        if self.chain[i].kind == ScopeKind::FunDecl {
                            self.symbols[symbol_id].promote_as_upvalue();
                        }
                    }
                }
                return Some(symbol_id);
            }
        }
        None
    }
}
