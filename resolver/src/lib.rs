/// Performs name resolution on a raw AST,
/// in order to build another AST with its name resolved _and_ a symbol table.
mod resolve;
mod scopes;
mod symbols;

pub use resolve::{resolve_names, Ast, NameError};
use scopes::{Globals, ScopeChain};
pub use symbols::{Sym, Symbol, SymbolId, SymbolTable};
