use crate::block_headers::{BlockHeader, BLOCK_HEADER_SIZE_IN_LINE};
use crate::heap_objects::Header;
use std::alloc::{alloc, dealloc, Layout};
use std::ops::Add;
use std::ptr::NonNull;

pub type BlockPtr = NonNull<u8>;
pub type InBlockPtr = NonNull<u8>;

#[derive(Debug)]
pub enum BlockError {
    /// User request a size which is not a power of two
    BadRequest,
    /// global alloc() returned NULL
    OOM,
    /// User requested an object bigger than a Block
    TooBig,
}

/// use 2**15 bytes for each block (32 kB)
pub const BLOCK_SIZE: usize = 1 << 15;

/// use 2**7 bytes for each line
pub const LINE_SIZE_BITS: usize = 7;
/// use lines of 128 bytes
pub const LINE_SIZE: usize = 1 << LINE_SIZE_BITS;
/// number of line per block, 256.
pub const LINE_COUNT: usize = BLOCK_SIZE / LINE_SIZE;

/// Block of allocated data.
/// It also keeps track of which sections (lines) are full.
/// It does it in a "Header", written at the very start
/// of the data block.
pub struct Block {
    /// pointer to a block of memory:
    /// * garanteed to be aligned with `BLOCK_SIZE`.
    /// * of size `BLOCK_SIZE`
    ptr: BlockPtr,
    /// offset from the start of the block,
    /// to the *start* of a free chunk of data.
    cursor: BlockOffset,
    /// offset from the start of the block,
    /// to the *end* of a free chunk of data.
    limit: BlockOffset,
}

impl Block {
    /// Allocate a block, **aligned on its size**
    /// size MUST be a power of two.
    /// (delegate the allocation to the global allocator).
    fn alloc_block() -> Result<BlockPtr, BlockError> {
        if !BLOCK_SIZE.is_power_of_two() {
            return Err(BlockError::BadRequest);
        }
        // SAFETY:
        // * the size check is of `size` is already done.
        // * We check for null right before calling `NonNull::new_unchecked()`
        unsafe {
            let layout = Layout::from_size_align_unchecked(BLOCK_SIZE, BLOCK_SIZE);
            let ptr = alloc(layout);
            if ptr.is_null() {
                return Err(BlockError::OOM);
            }
            Ok(std::ptr::NonNull::new_unchecked(ptr))
        }
    }

    /// (delegate the deallocation to the global allocator).
    fn dealloc_block(&self) {
        // SAFETY:
        // * the size check is of `size` is done at the allocation
        unsafe {
            let layout = Layout::from_size_align_unchecked(BLOCK_SIZE, BLOCK_SIZE);
            dealloc(self.ptr.as_ptr(), layout);
        }
    }

    /// returns a pointer to a slice of bytes
    /// that can contains an object of `alloc_size`.
    pub(crate) fn claim_slot(&mut self, alloc_size: usize) -> Option<InBlockPtr> {
        let mut next_bump = self.cursor + alloc_size;
        // check is the object would fit in the empty slice pointed by self.cursor
        // if not, lookup for the next hole
        while next_bump > self.limit {
            if !next_bump.in_block() {
                return None;
            }
            match self.header().find_next_available_hole(self.limit) {
                Some((cursor, limit)) => {
                    self.cursor = cursor;
                    self.limit = limit;
                    next_bump = cursor + alloc_size;
                }
                None => return None,
            }
        }
        let offset = self.cursor;
        self.cursor = next_bump;
        Some(offset.absolute_ptr(&*self))
    }

    /// Allocate a new block of size `size`
    pub(crate) fn allocate() -> Result<Self, BlockError> {
        let mut block = Self {
            ptr: Self::alloc_block()?,
            // first lines contains header
            cursor: BlockOffset::from_line_index(BLOCK_HEADER_SIZE_IN_LINE),
            limit: BlockOffset::new(BLOCK_SIZE),
        };
        // intialize block header by zero-ing it
        block.header_mut().clear();
        Ok(block)
    }

    /// return ref to the BlockHeader
    /// (the first LINE_SIZE bytes of the block)
    pub(crate) fn header(&self) -> &BlockHeader {
        unsafe {
            let header = self.ptr.as_ptr().cast::<BlockHeader>();
            &*header
        }
    }

    /// return exclusize ref to the BlockHeader
    /// (the first LINE_SIZE bytes of the block)
    pub(crate) fn header_mut(&mut self) -> &mut BlockHeader {
        unsafe {
            let header = self.ptr.as_ptr().cast::<BlockHeader>();
            &mut *header
        }
    }

    /// return in a state as if the block did not contain any live
    /// object, without actually touching the objects.
    pub(crate) fn clear(&mut self) {
        self.header_mut().clear();
    }

    /// find the first hole in the block
    pub(crate) fn recompute_limits(&mut self) {
        let offset_to_data = BlockOffset::from_line_index(BLOCK_HEADER_SIZE_IN_LINE);
        match self.header().find_next_available_hole(offset_to_data) {
            Some((cursor, limit)) => {
                self.cursor = cursor;
                self.limit = limit;
            }
            None => {
                self.cursor = BlockOffset::new(BLOCK_SIZE);
                self.limit = self.cursor;
            }
        }
    }
}
impl Drop for Block {
    /// delegate work to `self.dealloc_block()`
    fn drop(&mut self) {
        self.dealloc_block();
    }
}

/// an offset address, relative to a `Block` start address
#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Copy)]
pub(crate) struct BlockOffset(usize);
impl BlockOffset {
    /// returns the absolute address of the data pointed by `self`
    pub(crate) fn absolute_ptr(&self, block: &Block) -> InBlockPtr {
        unsafe {
            let self_ptr = block.ptr.as_ptr().add(self.0);
            // block.ptr is non-null => so is self_ptr
            NonNull::<u8>::new_unchecked(self_ptr)
        }
    }

    /// return the index of the line that contains self in a block.
    pub(crate) fn line_index(&self) -> usize {
        self.0 >> LINE_SIZE_BITS
    }

    /// return an offset relative to the start of the block,
    /// from a block line index.
    pub(crate) fn from_line_index(index: usize) -> Self {
        Self(index << LINE_SIZE_BITS)
    }

    pub(crate) fn in_block(&self) -> bool {
        self.0 < BLOCK_SIZE
    }

    pub(crate) fn new(offset: usize) -> Self {
        Self(offset)
    }
}

impl Add<Self> for BlockOffset {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0)
    }
}
impl Add<usize> for BlockOffset {
    type Output = Self;

    fn add(self, other: usize) -> Self {
        Self(self.0 + other)
    }
}
