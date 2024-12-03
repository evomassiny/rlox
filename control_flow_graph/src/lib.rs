use resolver::{Sym, Symbol, SymbolId, SymbolTable};
pub type BlockID = usize;

pub enum Terminator {
    If { then_target: BlockID, else_target: NodeId },
    Goto { target: BlockID },
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
