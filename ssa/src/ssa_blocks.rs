use resolver::{SymbolId};
use std::collections::HashMap;
use parser::{NodeId};

pub type SsaId = usize;

/// all (static single) assignment within a block
pub struct BlockLocals {
    symbols_to_ssa: HashMap<SymbolId, SsaId>,
    /// Id of the block statement,
    /// we use it to track provenance of each declaration
    block_stmt_id: NodeId,
}
impl BlockLocals {
    pub fn new(block_stmt_id: NodeId) -> Self {
        Self { 
            symbols_to_ssa: HashMap::new(),
            block_stmt_id,
        }
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
    traversed_locals: Vec<(BlockKind, BlockLocals)>,
    /// stores the mapping between SsaId and their corresponding SymbolId,
    /// along with the NodeId of the statement that declared them.
    /// Those NodeId are essential to track the provenance of
    /// assignment in phi nodes.
    vars: Vec<(SymbolId, NodeId)>,
}

impl AssignmentsChain {
    pub fn new() -> Self {
        Self { 
            traversed_locals: Vec::new(),
            vars: Vec::new(),
        }
    }

    pub fn push(&mut self, kind: BlockKind, block_stmt_id: NodeId) {
        let locals = BlockLocals::new(block_stmt_id);
        self.traversed_locals.push((kind, locals));
    }

    /// Assign a variable to a block scope.
    /// Allocate a new Static Single Assigment Id
    /// to identify the assigment.
    pub fn assign(&mut self, var_id: &SymbolId) ->  SsaId {
        let ssa_id: SsaId = self.vars.len();
        match self.traversed_locals.last_mut() {
            Some((_, locals)) => {
                // register the assignment in the global table
                self.vars.push((*var_id, locals.block_stmt_id));
                // register the assignment in the local scope/block
                locals.symbols_to_ssa.insert(*var_id, ssa_id);
            },
            None => unreachable!("Need a block/scope to register an assignement."),
        };
        ssa_id
    }

    /// resolve a variable SsaId from its SymbolId
    /// given the state of the AST traversal.
    pub fn resolve(&self, var_id: &SymbolId) -> SsaId {
        for (_, locals) in self.traversed_locals.iter().rev() {
            match locals.symbols_to_ssa.get(var_id) {
                Some(ssa_id) => return *ssa_id,
                None => continue
            }
        }
        unreachable!("The (previous) name resolution pass asserts that all variables are defined");
    }

    pub fn pop(&mut self) {
        // pop last local,
        // merge it to the previous one,
        // and return the list of assignment merges that should be performed
        self.traversed_locals.pop();
        todo!();
    }
}


