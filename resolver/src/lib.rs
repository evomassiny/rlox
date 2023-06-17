/// Performs name resolution on a raw AST,
/// in order to build another AST with its name resolved _and_ a symbol table.
mod resolve;
mod symbols;

pub use resolve::{resolve_names, Ast};
pub use symbols::{Symbol, SymbolId};
