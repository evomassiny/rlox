use crate::blocks::{BlockOffset, BLOCK_SIZE, LINE_COUNT, LINE_SIZE, LINE_SIZE_BITS};
use crate::heap_objects::Header;
use std::mem::size_of;

/// Number of lines taken by the block header
pub const BLOCK_HEADER_SIZE_IN_LINE: usize = (size_of::<BlockHeader>() + LINE_SIZE - 1) / LINE_SIZE;

// Because blocks are aligned on their size,
// removing the lower bit of an allocated object
// will return the addr of the header it belongs to.
pub const MASK_LOWER_BLOCK_BITS: usize = !(BLOCK_SIZE - 1);
/// (*object) & MASK_UPPER_BLOCK_BITS => offset from object start
pub const MASK_UPPER_BLOCK_BITS: usize = BLOCK_SIZE - 1;

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
    /// when the GC decide to evacuate this block, we flag it here.
    pub evacuating: bool,
    pub hole_count: usize,
}
impl BlockHeader {
    /// starting from the offset `starting_at` locate the next hole,
    /// based on the marks of `self.line_marks`
    /// return its start and end offset
    pub fn find_next_available_hole(
        &self,
        starting_at: BlockOffset,
    ) -> Option<(BlockOffset, BlockOffset)> {
        let mut current_line = starting_at.line_index();

        // loop over markers, to find the start of an hole
        'scan_for_start: loop {
            if current_line >= LINE_COUNT {
                return None;
            }
            if self.line_marks[current_line] {
                current_line += 1;
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
        let start = BlockOffset::new(byte_offset);
        let end = start + object_size;
        for line_idx in start.line_index()..=end.line_index() {
            // SAFETY:
            // because of the above mask, we cannot be
            // out of bound.
            unsafe {
                *self.line_marks.get_unchecked_mut(line_idx) = true;
            }
        }
    }

    /// reset line marks and hole count, as if the block contained no live object
    pub fn reset(&mut self) {
        // the first lines of the block are taken by `self`,
        // so it's never free
        for line_idx in 0..BLOCK_HEADER_SIZE_IN_LINE {
            self.line_marks[line_idx] = true;
        }
        // rest of lines are unmarked
        for line_idx in BLOCK_HEADER_SIZE_IN_LINE..LINE_COUNT {
            self.line_marks[line_idx] = false;
        }
        // if the block is free, it only contains one big hole
        self.hole_count = 1;
        self.evacuating = false;
    }

    /// Returns
    /// * the number of holes (nb of group of contiguous unmarked lines),
    /// * the number of marked lines
    pub fn count_holes_and_marked_lines(&self) -> (usize, usize) {
        let mut hole_count = 0;
        let mut marked_count = 0;
        let mut previous_was_marked = true;
        for line_idx in BLOCK_HEADER_SIZE_IN_LINE..LINE_COUNT {
            if self.line_marks[line_idx] {
                marked_count += 1;
            }
            if !self.line_marks[line_idx] && previous_was_marked {
                hole_count += 1;
            }
            previous_was_marked = self.line_marks[line_idx];
        }
        (hole_count, marked_count)
    }
}

#[test]
fn block_header_size() {
    // just so i don't forget to maintain the `BLOCK_HEADER_SIZE_IN_LINE` const.
    // If its not right, resetting the BlockHeader mutate the first object
    // stored in the block, with leads to confusing errors.
    assert!(size_of::<BlockHeader>() <= (LINE_SIZE * BLOCK_HEADER_SIZE_IN_LINE));
}

#[test]
fn hole_lookup() {
    let mut block_header = BlockHeader {
        line_marks: [false; LINE_COUNT],
        hole_count: 0,
        evacuating: false,
    };
    block_header.reset();

    block_header.line_marks[10] = true; // mark line as "filled"
    assert_eq!(
        block_header.find_next_available_hole(BlockOffset::new(0)),
        Some((
            BlockOffset::from_line_index(BLOCK_HEADER_SIZE_IN_LINE),
            BlockOffset::from_line_index(10)
        )),
    );

    // assert that a marked line also invalidate the following one.
    block_header.line_marks[15] = true;
    assert_eq!(
        block_header.find_next_available_hole(BlockOffset::from_line_index(10)),
        Some((
            BlockOffset::from_line_index(11),
            BlockOffset::from_line_index(15),
        )),
    );
}
