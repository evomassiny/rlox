use resolver::{Sym, Symbol, SymbolId, SymbolTable};
pub type BlockID = usize;

#[derive(Debug, PartialEq)]
pub struct Stmt<Symbol> {
    /// An id identifiying the AST node
    pub id: NodeId,
    /// All statement variants
    pub kind: StmtKind,
    /// position in source string
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    /// List of inner statements
    Block(Vec<Stmt>),
    /// Class name, super class variable name, functions
    Class(Symbol, Option, Vec<Stmt>),
    /// condition, then branch, else branch
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    /// function name, args, body
    Function(Symbol, Vec, Vec<Stmt>),
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Return(Option<Box<Expr>>),
    /// var name, initializer
    Var(Symbol, Box<Expr>),
    /// condition, body
    While(Box<Expr>, Box<Stmt>),
    /// initializer, condition, increment, body
    For(
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Box<Stmt>,
    ),
}

pub enum Terminator {
    If {
        then_target: BlockID,
        else_target: BlockID,
    },
    Goto {
        target: BlockID,
    },
    Return,
}

pub struct BasicBlock {
    pub id: BlockID,
    pub statements: Vec<Stmt<Sym>>,
    pub terminator: Option<Terminator>,
}

pub struct ControlFlowGraph {
    pub symbols: SymbolTable,
    pub blocks: Vec<BasicBlock>,
    pub start: BlockID,
}

#[cfg(test)]
mod tests {
    use super::*;
}
