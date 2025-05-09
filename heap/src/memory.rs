use crate::block_headers::BlockState;
use crate::blocks::{Block, InBlockPtr, LINE_COUNT};

/// in a block, the max number of holes
/// occurs when one line every two line is marked,
pub const MAX_NB_OF_HOLE_IN_BLOCK: usize = (LINE_COUNT + 1) / 2;

/// round up to nearest multiple of 8.
#[inline(always)]
fn round_up_to_8_bytes_aligned(size: usize) -> usize {
    0xfffffffffffffff8 & (size + 0x7)
}

#[derive(Debug)]
pub enum MemoryError {
    OOM,
    TooBig,
    NoFreeBlock,
}

/// Bunch of memory blocks
pub struct Memory {
    // Each block contains pointer to allocated blocks of data
    pub(crate) blocks: Vec<Block>,
}

impl Memory {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub(crate) fn alloc(
        &mut self,
        alloc_size: usize,
    ) -> Result<InBlockPtr, MemoryError> {
        // align size on 8 bytes, as x86_64 proc don't
        // allow misaligned memory access.
        let alloc_size = round_up_to_8_bytes_aligned(alloc_size);

        // linearly search for an empty slot big enough for `alloc_size`
        for block in self.blocks.iter_mut() {
            if let Some(address) = block.claim_slot(alloc_size) {
                return Ok(address);
            }
        }
        // if None was found, claim a new block of memory
        let mut block = match Block::allocate() {
            Ok(block) => block,
            Err(_) => return Err(MemoryError::OOM),
        };
        // and allocate into it
        let address = match block.claim_slot(alloc_size) {
            Some(address) => address,
            None => return Err(MemoryError::TooBig),
        };
        self.blocks.push(block);
        Ok(address)
    }

    /// Build a mapping between blocks (grouped by "hole count")
    /// and the number of free line in the group.
    pub fn compute_availabily_histogram(
        &mut self,
    ) -> [usize; MAX_NB_OF_HOLE_IN_BLOCK] {
        let mut hist: [usize; MAX_NB_OF_HOLE_IN_BLOCK] =
            [0; MAX_NB_OF_HOLE_IN_BLOCK];
        for block in self.blocks.iter() {
            match block.header().state {
                BlockState::PartiallyFull {
                    hole_count,
                    mark_count,
                } => hist[hole_count] += LINE_COUNT - mark_count,
                _ => continue,
            }
        }
        hist
    }

    /// re-allocate an object into an Existing block,
    /// prefering "PartiallyFull" blocks
    /// and avoiding "Evacuating" one.
    pub(crate) fn evacuate(
        &mut self,
        alloc_size: usize,
    ) -> Result<InBlockPtr, MemoryError> {
        // linearly search for an empty slot big enough for `alloc_size`
        // preferably in "PartiallyFull" blocks
        for block in self.blocks.iter_mut() {
            if !matches!(block.header().state, BlockState::PartiallyFull { .. })
            {
                continue;
            }
            if let Some(address) = block.claim_slot(alloc_size) {
                return Ok(address);
            }
        }
        // if no slot has been found, search in Free ones
        for block in self.blocks.iter_mut() {
            if !matches!(block.header().state, BlockState::Free) {
                continue;
            }
            if let Some(address) = block.claim_slot(alloc_size) {
                return Ok(address);
            }
        }
        Err(MemoryError::NoFreeBlock)
    }
}

#[test]
fn test_size_alignment() {
    assert_eq!(round_up_to_8_bytes_aligned(8), 8);
    assert_eq!(round_up_to_8_bytes_aligned(7), 8);
    assert_eq!(round_up_to_8_bytes_aligned(1), 8);
    assert_eq!(round_up_to_8_bytes_aligned(0), 0);
    assert_eq!(round_up_to_8_bytes_aligned(15), 16);
}
