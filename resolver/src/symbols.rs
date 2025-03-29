use lexer::Span;
use std::ops::{Index, IndexMut};

/// represents how a value should be
/// stored in memory
#[derive(Debug, PartialEq, Eq)]
pub enum StorageKind {
    /// includes functions locals and globals
    StackLocal,
    /// function locals captured by closures.
    /// (in the program execution, such variable must be boxed)
    UpValue,
    // TODO: what about object attributes ?
}

/// Represents a variable binding
#[derive(Debug)]
pub struct Symbol {
    /// name of the variable (how it's been declared)
    pub name: String,
    /// where it's been declared
    pub src: Span,
    /// how we should store it
    pub storage_kind: StorageKind,
}

impl Symbol {
    /// Mark a symbol as `upvalue`, eg: a variable captured by
    /// a closure.
    pub fn promote_as_upvalue(&mut self) {
        self.storage_kind = StorageKind::UpValue;
    }
}

/// Index a symbol
/// (new type pattern over usize)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolId(usize);

/// A table of Symbols,
/// indexable by a `SymbolId`
#[derive(Debug)]
pub struct SymbolTable {
    // the table is simply a Vec,
    // it is wrapped in a Struct which implements
    // indexing, so we can move to some other kind of map if needed.
    pub symbols: Vec<Symbol>,
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
        let symbol = Symbol {
            name,
            src,
            storage_kind: StorageKind::StackLocal,
        };
        self.symbols.push(symbol);
        SymbolId(id)
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
    }
}

impl Index<SymbolId> for SymbolTable {
    type Output = Symbol;

    fn index(&self, index: SymbolId) -> &Self::Output {
        &self.symbols[index.0]
    }
}
impl IndexMut<SymbolId> for SymbolTable {
    fn index_mut(&mut self, index: SymbolId) -> &mut Self::Output {
        &mut self.symbols[index.0]
    }
}

impl Index<&SymbolId> for SymbolTable {
    type Output = Symbol;

    fn index(&self, index: &SymbolId) -> &Self::Output {
        &self.symbols[(*index).0]
    }
}

impl IndexMut<&SymbolId> for SymbolTable {
    fn index_mut(&mut self, index: &SymbolId) -> &mut Self::Output {
        &mut self.symbols[(*index).0]
    }
}

impl std::fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut repr = String::new();
        for symbol in &self.symbols {
            repr += &format!(
                "{0}: {2:?} (l.{1})\n",
                symbol.name, symbol.src.line, symbol.storage_kind
            );
        }
        write!(f, "{}", repr)
    }
}
