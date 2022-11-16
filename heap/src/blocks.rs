use std::alloc::{alloc, dealloc, Layout};
use std::ops::Add;

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
    /// Allocate a block, **aligned on its size**
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
        let mut block = Self {
            ptr: Self::alloc_block(size)?,
            size,
        };
        // intialize block header by zero-ing it
        for line_mark in block.header_mut().line_mark.iter_mut() {
            *line_mark = false;
        }
        // mark the first line as full,
        // because it contains the BlockHeader itself.
        block.header_mut().line_mark[0] = true;
        Ok(block)
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.ptr.as_ptr()
    }

    /// return ref to the BlockHeader
    /// (the first LINE_SIZE bytes of the block)
    pub fn header(&self) -> &BlockHeader {
        unsafe {
            std::mem::transmute::<*const u8, &BlockHeader>(self.ptr.as_ptr())
        }
    }
    
    /// return exclusize ref to the BlockHeader
    /// (the first LINE_SIZE bytes of the block)
    pub fn header_mut(&mut self) -> &mut BlockHeader {
        unsafe {
            std::mem::transmute::<*const u8, &mut BlockHeader>(self.ptr.as_ptr())
        }
    }
}
impl Drop for Block {
    /// delegate work to `self.dealloc_block()`
    fn drop(&mut self) {
        self.dealloc_block();
    }
}

/// use 2**15 bytes for each block (32 kB)
pub const BLOCK_SIZE: usize = 1 << 15;

// Because blocks are aligned on their size,
// removing the lower bit of an alloacted object 
// will return the addr of the header it belongs to.
pub const MASK_LOWER_BLOCK_BITS: usize = !(BLOCK_SIZE - 1);

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

/// BlockHeader, first bytes of an HEADER
#[repr(C)]
pub struct BlockHeader {
    /// keeps track of which section (called line) contains live objects
    pub line_mark: [bool; LINE_COUNT],
}
impl BlockHeader {
    /// starting from the offset `starting_at` locate the next hole,
    /// based on the marks of `self.line_mark`
    /// return its start and end offset
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

    /// Return the addr of the block header related to the object pointed by `ptr`
    /// (rely on block alignement for this)
    /// 
    /// NOTE:
    /// the lifetime of the returned value is not actually 'static,
    /// it is tied to the lifetime of its block.
    /// But the compiler does not know that.
    pub unsafe fn from_object_ptr(ptr: *const u8) -> &'static BlockHeader {
        // Because blocks are aligned on their size,
        // removing the lower bit of an alloacted object 
        // will return the addr of the header it belongs to.

        let block_start = (ptr as usize) & MASK_LOWER_BLOCK_BITS;
        std::mem::transmute::<*const u8, &BlockHeader>(block_start as *const u8)

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
            match self.find_next_available_hole(self.limit) {
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

    /// lookup for unmarker lines in the block header
    pub fn find_next_available_hole(
        &self,
        starting_at: BlockOffset,
    ) -> Option<(BlockOffset, BlockOffset)> {
        self.block.header().find_next_available_hole(starting_at)
    }

    pub fn new() -> Result<Self, BlockError> {
        Ok(Self {
            cursor: BlockOffset::new(0),
            limit: BlockOffset::new(BLOCK_SIZE),
            block: Block::new(BLOCK_SIZE)?,
        })
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
    let mut block = BumpBlock::new().unwrap();
    block.block.header_mut().line_mark[10] = true; // mark line as "filled"
    assert_eq!(
        block.find_next_available_hole(BlockOffset::new(0)),
        Some((
            BlockOffset::from_line_index(2),
            BlockOffset::from_line_index(10)
        )),
    );

    // assert that a marked line also invalidate the following one.
    block.block.header_mut().line_mark[15] = true;
    assert_eq!(
        block.find_next_available_hole(BlockOffset::from_line_index(10)),
        Some((
            BlockOffset::from_line_index(12),
            BlockOffset::from_line_index(15),
        )),
    );
}
