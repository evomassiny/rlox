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
    //pub type_constraints: Vec<TypeConstraint>,
}

impl Symbol {
    /// Mark a symbol as `upvalue`, eg: a variable captured by
    /// a closure.
    pub fn promote_as_upvalue(&mut self) {
        self.storage_kind = StorageKind::UpValue;
    }
}

pub type SymbolId = usize;

/// either a direct
/// reference to a given location, or an alternation between
/// several variants.
#[derive(Debug)]
pub enum Sym {
    // ref a know location in memory
    Direct(SymbolId),
    // Globals ! depend of callsite
    OneOf(Box<[SymbolId]>),
}

/// A table of Symbols,
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
        let symbol = Symbol {
            name,
            src,
            storage_kind: StorageKind::StackLocal,
        };
        self.symbols.push(symbol);
        id
    }

    pub fn len(&self) -> usize {
        self.symbols.len()
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
