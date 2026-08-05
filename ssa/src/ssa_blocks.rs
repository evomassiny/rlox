use resolver::{SymbolId};
use std::collections::HashMap;

pub type SsaId = usize;

/// all (static single) assignement within a block
pub struct BlockLocals {
    ssa_symbols: HashMap<SymbolId, SsaId>,
}
impl BlockLocals {
    pub fn new() -> Self {
        Self { ssa_symbols: HashMap::new() }
    }
}

/// every block kind we can encounter while traversing the AST
pub enum BlockKind {
    WhileBody,
    ThenBody,
    ElseBody,
    Block,
}

/// State of SSA traversal pass
pub struct AssignmentsChain {
    /// stack of blocks, as we traverse them.
    blocks: Vec<(BlockKind, BlockLocals)>,
}

impl AssignmentsChain {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn push(&mut self, kind: BlockKind) {
        let locals = BlockLocals::new();
        self.blocks.push((kind, locals));
    }

    pub fn pop(&mut self) {
        // pop last local,
        // merge it to the previous one,
        // and return the list of assignment merges that should be performed
        todo!();
    }
}


