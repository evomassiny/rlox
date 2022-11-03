use std::alloc::{alloc, dealloc, Layout};
use std::ops::Add;

/// Align on 2 words boundaries, for simplicity
pub const ALIGNMENT: usize = std::mem::align_of::<usize>() * 2;

type BlockPtr = std::ptr::NonNull<u8>;
type BlockSize = usize;

#[derive(Debug)]
pub enum BlockError {
    /// User request a size which is not a power of two
    BadRequest,
    /// global alloc() returned NULL
    OOM,
    /// User requested an object bigger than a Block
    TooBig,
}

/// This struct represents a Block of allocated data.
/// It's meant to be a safe interface around alloc/dealloc
pub struct Block {
    /// pointer to block data,
    /// garanteed to be aligned with `size`.
    ptr: BlockPtr,
    /// size (in bytes) or the data block,
    /// garenteed to be a power of two.
    size: BlockSize,
}

impl Block {
    /// Allocate a block, aligned on its size.
    /// size MUST be a power of two.
    /// (delegate the allocation to the global allocator).
    fn alloc_block(size: BlockSize) -> Result<BlockPtr, BlockError> {
        if !size.is_power_of_two() {
            return Err(BlockError::BadRequest);
        }
        // SAFETY:
        // * the size check is of `size` is already done.
        // * We check for null right before calling `NonNull::new_unchecked()`
        unsafe {
            let layout = Layout::from_size_align_unchecked(size, size);
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
            let layout = Layout::from_size_align_unchecked(self.size, self.size);
            dealloc(self.ptr.as_ptr(), layout);
        }
    }

    /// Allocate a new block of size `size`
    pub fn new(size: BlockSize) -> Result<Self, BlockError> {
        Ok(Self {
            ptr: Self::alloc_block(size)?,
            size,
        })
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }
}
impl Drop for Block {
    /// delegate work to `self.dealloc_block()`
    fn drop(&mut self) {
        self.dealloc_block();
    }
}

/// use 2**15 bytes for each block
pub const BLOCK_SIZE_BITS: usize = 15;
/// use 32 kB block
pub const BLOCK_SIZE: usize = 1 << BLOCK_SIZE_BITS;

/// use 2**7 bytes for each line
pub const LINE_SIZE_BITS: usize = 7;
/// use lines of 128 bytes
pub const LINE_SIZE: usize = 1 << LINE_SIZE_BITS;
/// number of line per block, 256.
pub const LINE_COUNT: usize = BLOCK_SIZE / LINE_SIZE;

/// an offset address, relative to a `Block` start address
#[derive(PartialEq, Eq, Debug, PartialOrd, Ord, Clone, Copy)]
pub struct BlockOffset(usize);
impl BlockOffset {
    /// returns the absolute address of the data pointed by `self`
    pub fn as_ptr(&self, block: &Block) -> *const u8 {
        unsafe { block.as_ptr().add(self.0) }
    }

    /// return the index of the line that contains self in a block.
    pub fn line_index(&self) -> usize {
        self.0 >> LINE_SIZE_BITS
    }

    /// return an offset relative to the start of the block,
    /// from a block line index.
    pub fn from_line_index(index: usize) -> Self {
        Self(index << LINE_SIZE_BITS)
    }

    pub fn in_block(&self) -> bool {
        self.0 < BLOCK_SIZE
    }

    pub fn new(offset: usize) -> Self {
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

/// Bump allocator, allocates into an underliying `Block` of data.
/// It divides each blocks into "lines", everytime we allocate an object into
/// the block, we mark which sections (eg. lines) of the block were affected.
pub struct BumpBlock {
    /// offset to self.block.as_ptr() that points
    /// the next object can be written (in the block)
    /// eg: it point to an "empty" section of data,
    /// the size of this empty section is decribed by `self.limit`
    cursor: BlockOffset,
    /// size of the "empty" section pointed by `self.cursor`
    limit: BlockOffset,
    /// the underlying allocated array
    block: Block,
    /// object that keeps track of which section contains live objects
    meta: Box<BlockMeta>,
}

impl BumpBlock {
    /// returns a pointer to a slice of bytes that can contains an object of `alloc_size`.
    pub fn inner_alloc(&mut self, alloc_size: usize) -> Option<*const u8> {
        let mut next_bump = self.cursor + alloc_size;
        // check is the object would fit in the empty slice pointed by self.cursor
        // if so, lookup for the next hole
        while next_bump > self.limit {
            if !next_bump.in_block() {
                return None;
            }
            match self.meta.find_next_available_hole(self.limit) {
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
        Some(offset.as_ptr(&self.block))
    }

    pub fn new() -> Result<Self, BlockError> {
        Ok(Self {
            cursor: BlockOffset::new(0),
            limit: BlockOffset::new(BLOCK_SIZE),
            meta: Box::new(BlockMeta::new()),
            block: Block::new(BLOCK_SIZE)?,
        })
    }
}

/// Keeps tracks of marked lines in a block
pub struct BlockMeta {
    /// either or not each contains live objects
    pub line_mark: [bool; LINE_COUNT],
    /// either or not the whole block contains live objects
    pub block_mark: bool,
}
impl BlockMeta {
    /// starting from the offset `starting_at` locate the next hole,
    /// return its start and end offset.
    pub fn find_next_available_hole(
        &self,
        starting_at: BlockOffset,
    ) -> Option<(BlockOffset, BlockOffset)> {
        // looks for a window of N lines which are unmarked

        // same as starting_at / LINE_SIZE
        let mut current_line = starting_at.line_index();

        // loop over markers, to find the start of an hole
        'scan_for_start: loop {
            if current_line >= LINE_COUNT {
                return None;
            }
            if self.line_mark[current_line] {
                // skip one line,
                // When we mark a line, we consider that the object
                // that it contains might spill into the next one.
                // This means that each
                current_line += 2;
                continue 'scan_for_start;
            }
            break;
        }
        let hole_start = BlockOffset::from_line_index(current_line);

        // then locate the end of the hole
        let mut hole_size: usize = 1;
        'scan_for_end: loop {
            current_line += 1;
            if current_line >= LINE_COUNT || self.line_mark[current_line] {
                break 'scan_for_end;
            }
            hole_size += 1;
        }
        let hole_end = hole_start + BlockOffset::from_line_index(hole_size);
        Some((hole_start, hole_end))
    }

    pub fn new() -> Self {
        Self {
            line_mark: [false; LINE_COUNT],
            block_mark: false,
        }
    }
}

#[test]
fn alloc_and_dealloc_block() {
    let size = 1024;
    let block = Block::new(size).unwrap();
    // the block address bitwise AND the alignment bits (size - 1) should
    // be a mutually exclusive set of bits
    let mask = size - 1;
    assert!((block.as_ptr() as usize & mask) ^ mask == mask);
}

#[test]
fn hole_lookup() {
    let mut meta = BlockMeta::new();
    meta.line_mark[10] = true; // mark line as "filled"
    assert_eq!(
        meta.find_next_available_hole(BlockOffset::new(0)),
        Some((BlockOffset::from_line_index(0), BlockOffset::from_line_index(10))),
    );

    // assert that a marked line also invalidate the following one.
    meta.line_mark[15] = true;
    assert_eq!(
        meta.find_next_available_hole(BlockOffset::from_line_index(10)), 
        Some((
                BlockOffset::from_line_index(12),
                BlockOffset::from_line_index(15),
            )),
    );
}
