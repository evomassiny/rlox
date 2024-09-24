use super::symbols::{StorageKind, Sym, SymbolId, SymbolTable};
use lexer::Span;
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
        if let Some(globals) = self.symbols.get(name) {
            if globals.len() == 0 {
                return Some(Sym::Direct(globals[0]));
            }
            return Some(Sym::OneOf(globals.clone().into_boxed_slice()));
        }
        None
    }
}

/// When we traverse the AST, we end up
/// encountering 3 kinds of scope,
/// keeping track of them allow us to
/// distinguish captured variable from the others,
/// and resolve the type of `super` and `this`
#[derive(Debug, PartialEq, Eq)]
pub enum ScopeKind {
    Block,
    FunDecl,
    ClassDecl,
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

    pub fn add(&mut self, name: String, src: Span, table: &mut SymbolTable) -> SymbolId {
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
    // holds all globals
    globals: Globals,
    // a stack which should mimick the nesting of scopes
    // we are traversing.
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

    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.chain.push(Scope::new(kind));
    }

    pub fn pop_scope(&mut self) {
        let _ = self.chain.pop();
    }

    pub fn add(&mut self, name: String, src: Span) -> Sym {
        let symbol_id = match self.chain.last_mut() {
            // inner scope
            Some(scope) => scope.add(name, src, self.symbols),
            // We could append the binding directly into the global scope,
            // _but_ it should have been previously resolved, in a dedicated pass
            None => match self.globals.resolve_precise(&name, &src, self.symbols) {
                Some(symbol_id) => symbol_id,
                None => {
                    unreachable!("the global binding '{name}' should have already been resolved")
                }
            },
        };
        Sym::Direct(symbol_id)
    }

    /// traverse the scope chain to find the nearest declaration of `name`
    /// In the process, promote variable as upvalues if needed.
    pub fn resolve(&mut self, name: &str) -> Option<Sym> {
        // first lookup in the lexical scope chain
        for scope_idx in (0..self.chain.len()).rev() {
            if let Some(symbol_id) = self.chain[scope_idx].resolve(name) {
                // search for a function declaration scope
                // between where the variable was declared,
                // and the cureent usage.
                // If we found one, this means we're dealing with an
                // upvalue, eg: a variable captured by a closure.
                for i in scope_idx..self.chain.len() {
                    if self.chain[i].kind == ScopeKind::FunDecl {
                        self.symbols[symbol_id].promote_as_upvalue();
                    }
                }
                return Some(Sym::Direct(symbol_id));
            }
        }
        // fallback to globals
        self.globals.resolve(name, self.symbols)
    }
}
