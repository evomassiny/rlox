use lexer::Span;
use std::ops::{Index, IndexMut};

/// Represents a variable binding
#[derive(Debug)]
pub struct Symbol {
    pub name: String,
    pub src: Span,
}

pub type SymbolId = usize;

/// A table of Symbol,
/// indexable by a `SymbolId`
#[derive(Debug)]
pub struct SymbolTable {
    // the table is simply a Vec,
    // it is wrapped in a Struct which implements
    // indexing, so we can move to some other kind of map if needed.
    symbols: Vec<Symbol>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    /// creates an new symbol, add it to the table,
    /// and returns its ID
    pub fn add(&mut self, name: String, src: Span) -> SymbolId {
        let id = self.symbols.len();
        let symbol = Symbol { name, src };
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
    fn index_mut(&mut self, index: SymbolId) -> &mut Self::Output {
        &mut self.symbols[index]
    }
}

impl Index<&SymbolId> for SymbolTable {
    type Output = Symbol;

    fn index(&self, index: &SymbolId) -> &Self::Output {
        &self.symbols[*index]
    }
}

impl IndexMut<&SymbolId> for SymbolTable {
    fn index_mut(&mut self, index: &SymbolId) -> &mut Self::Output {
        &mut self.symbols[*index]
    }
}

