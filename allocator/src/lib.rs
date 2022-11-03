mod blocks;

use blocks::{Block, BlockError, BlockMeta, BlockOffset, BumpBlock, BLOCK_SIZE, LINE_COUNT};

pub struct BlockList {
    blocks: Vec<BumpBlock>,
}
impl BlockList {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn alloc(&mut self, alloc_size: usize) -> Result<*const u8, BlockError> {
        // linearly search for an empty slot big enough for `alloc_size`
        for block in self.blocks.iter_mut() {
            if let Some(address) = block.inner_alloc(alloc_size) {
                return Ok(address);
            }
        }
        // if None was found, claim a new block of memory
        let mut block = BumpBlock::new()?;
        // and allocate into it
        let address = match block.inner_alloc(alloc_size) {
            Some(address) => address,
            None => return Err(BlockError::TooBig),
        };
        self.blocks.push(block);
        Ok(address)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Block, BlockMeta, BlockOffset, LINE_COUNT};
}
