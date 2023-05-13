use std::ops::{Index, IndexMut};

/// Represents a variable binding
pub struct Symbol {
    name: String,
    id: SymbolId,
    scope: ScopeKind,
}

pub enum ScopeKind {
    Global,
    Local,
    Captured,
    Unresolved,
}

pub type SymbolId = usize;

/// A table of Symbol,
/// indexable by a `SymbolId`
pub struct SymbolTable {
    // the table is simply a Vec,
    // it is wrapped in a Struct which implement
    // indexing, so we can move to some other kind of map if needed.
    symbols: Vec<Symbol>,
}

impl SymbolTable {

    pub fn new() -> Self {
        Self {
            symbols: Vec::new()
        }
    }

    /// creates an new symbol, add it to the table,
    /// and returns its ID
    pub fn add(&mut self, name: String, scope: ScopeKind) -> SymbolId {
        let id = self.symbols.len();
        let symbol = Symbol {
            name,
            id,
            scope,
        };
        self.symbols.push(symbol);
        id
    }
}


impl Index<SymbolId> for SymbolTable {
    type Output = Symbol;

    fn index(&self, index: SymbolId) -> &Self::Output {
        &self.symbols[index]
    }
}

impl IndexMut<SymbolId> for SymbolTable {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.symbols[index]
    }
}

