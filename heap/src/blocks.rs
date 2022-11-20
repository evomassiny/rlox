use std::alloc::{alloc, dealloc, Layout};
use crate::heap_objects::{Header};
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

// Because blocks are aligned on their size,
// removing the lower bit of an allocated object
// will return the addr of the header it belongs to.
pub const MASK_LOWER_BLOCK_BITS: usize = !(BLOCK_SIZE - 1);
/// (*object) & MASK_UPPER_BLOCK_BITS => offset from object start
pub const MASK_UPPER_BLOCK_BITS: usize = BLOCK_SIZE - 1;

/// use 2**8 bytes for each line
pub const LINE_SIZE_BITS: usize = 8;
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

    /// returns a pointer to a slice of bytes that can contains an object of `alloc_size`.
    pub fn claim_slot(&mut self, alloc_size: usize) -> Option<InBlockPtr> {
        let mut next_bump = self.cursor + alloc_size;
        // check is the object would fit in the empty slice pointed by self.cursor
        // if so, lookup for the next hole
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
    pub fn allocate() -> Result<Self, BlockError> {
        let mut block = Self {
            ptr: Self::alloc_block()?,
            cursor: BlockOffset::new(LINE_SIZE), // first line contains header
            limit: BlockOffset::new(BLOCK_SIZE),
        };
        // intialize block header by zero-ing it
        for line_marks in block.header_mut().line_marks.iter_mut() {
            *line_marks = false;
        }
        // mark the first line as full,
        // because it contains the BlockHeader itself.
        block.header_mut().line_marks[0] = true;
        Ok(block)
    }

    /// return ref to the BlockHeader
    /// (the first LINE_SIZE bytes of the block)
    pub fn header(&self) -> &BlockHeader {
        unsafe {
            let header = self.ptr.as_ptr().cast::<BlockHeader>();
            &*header
        }
    }

    /// return exclusize ref to the BlockHeader
    /// (the first LINE_SIZE bytes of the block)
    pub fn header_mut(&mut self) -> &mut BlockHeader {
        unsafe {
            let header = self.ptr.as_ptr().cast::<BlockHeader>();
            &mut *header
        }
    }

    /// return in a state as if the block did not contain any live
    /// object, without actually touching the objects.
    pub fn reset_marks(&mut self) {
        self.header_mut().reset_marks();
        dbg!(std::mem::size_of::<BlockHeader>());
    }

    /// find the first hole in the block
    pub fn recompute_limits(&mut self) {
        match self.header().find_next_available_hole(self.limit) {
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
pub struct BlockOffset(usize);
impl BlockOffset {
    /// returns the absolute address of the data pointed by `self`
    pub fn absolute_ptr(&self, block: &Block) -> InBlockPtr {
        unsafe {
            let self_ptr = block.ptr.as_ptr().add(self.0);
            // block.ptr is non-null => so is self_ptr
            NonNull::<u8>::new_unchecked(self_ptr)
        }
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

/// BlockHeader, first bytes of a Block,
/// Contains an array of marks,
/// one per line.
/// If a line is "marked", it means that it contains
/// a live objects
#[derive(Debug)]
#[repr(C)]
pub struct BlockHeader {
    /// keeps track of which section (called line) contains live objects
    pub line_marks: [bool; LINE_COUNT],
}
impl BlockHeader {
    /// starting from the offset `starting_at` locate the next hole,
    /// based on the marks of `self.line_marks`
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
            if self.line_marks[current_line] {
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
            if current_line >= LINE_COUNT || self.line_marks[current_line] {
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
    pub unsafe fn from_object_ptr(ptr: *const Header) -> &'static mut BlockHeader {
        // Because blocks are aligned on their size,
        // removing the lower bit of an alloacted object
        // will return the addr of the header it belongs to.

        let block_start = (ptr as usize) & MASK_LOWER_BLOCK_BITS;
        let block_header_ptr = block_start as *mut BlockHeader;
        &mut *block_header_ptr
    }

    pub fn mark_lines(&mut self, object_ptr: *const Header, object_size: usize) {
        // because block are aligned, lower bits are equivalent to  byte offset.
        let byte_offset = (object_ptr as usize) & MASK_UPPER_BLOCK_BITS;
        // get line indices
        let start = byte_offset >> LINE_SIZE_BITS;
        let end = (byte_offset + object_size) >> LINE_SIZE_BITS;
        for line_idx in start..=end {
            // SAFETY:
            // because of the above mask, we cannot be
            // out of bound.
            unsafe {
                *self.line_marks.get_unchecked_mut(line_idx) = true;
            }
        }
    }

    pub fn reset_marks(&mut self) {
        // the first line is `self`, so it's never free
        self.line_marks[0] = true;
        for mark in self.line_marks.iter_mut() {
            *mark = false;
        }
    }
}

#[test]
fn block_header_size() {
    /// Assert that the Header fits in ONE line.
    assert!(std::mem::size_of::<BlockHeader>() <= LINE_SIZE);
}

#[test]
fn hole_lookup() {
    let mut block = Block::allocate().unwrap();
    block.header_mut().line_marks[10] = true; // mark line as "filled"
    assert_eq!(
        block.header().find_next_available_hole(BlockOffset::new(0)),
        Some((
            BlockOffset::from_line_index(2),
            BlockOffset::from_line_index(10)
        )),
    );

    // assert that a marked line also invalidate the following one.
    block.header_mut().line_marks[15] = true;
    assert_eq!(
        block
            .header()
            .find_next_available_hole(BlockOffset::from_line_index(10)),
        Some((
            BlockOffset::from_line_index(12),
            BlockOffset::from_line_index(15),
        )),
    );
}
