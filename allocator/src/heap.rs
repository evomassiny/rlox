use crate::blocks::{BlockError, BumpBlock};

#[derive(Debug)]
pub enum HeapError {
    OOM,
    TooBig,
}

/// GC-ed Heap
pub struct Heap {
    blocks: Vec<BumpBlock>,
}

impl Heap {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn alloc(&mut self, alloc_size: usize) -> Result<*const u8, HeapError> {
        // linearly search for an empty slot big enough for `alloc_size`
        for block in self.blocks.iter_mut() {
            if let Some(address) = block.inner_alloc(alloc_size) {
                return Ok(address);
            }
        }
        // if None was found, claim a new block of memory
        let mut block = match BumpBlock::new() {
            Ok(block) => block,
            Err(_) => return Err(HeapError::OOM),
        };
        // and allocate into it
        let address = match block.inner_alloc(alloc_size) {
            Some(address) => address,
            None => return Err(HeapError::TooBig),
        };
        self.blocks.push(block);
        Ok(address)
    }
}
