use crate::arrays::Array;
use crate::block_headers::{BlockHeader, BlockState};
use crate::blocks::{Block, BlockError, InBlockPtr, LINE_COUNT};
use crate::boxed_values::BoxedValue;
use crate::heap_objects::{Header, Markable, Object};
use crate::lists::List;
use crate::strings::Str;
use crate::tombstones::Tombstone;
use crate::values::Value;

#[derive(Debug)]
pub enum HeapError {
    OOM,
    TooBig,
}

/// in a block, the max number of holes
/// occurs when one line every two line is marked,
const MAX_NB_OF_HOLE: usize = (LINE_COUNT + 1) / 2;

/// GC-ed Heap
pub struct Heap {
    // Ecah block contains pointer to allocated blocks of data
    blocks: Vec<Block>,
    // total number of marked lines (accross all blocks),
    // indexed by the nb of hole in their respective block.
    // (used in the evacutation selection heuristic)
    // (called `mark histogram` in the original paper)
    marked_lines_by_holes_count: [usize; MAX_NB_OF_HOLE],
    // flag used to mark live object,
    // we flip it before each collection
    live_object_mark: bool,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            blocks: Vec::new(),
            marked_lines_by_holes_count: [0; MAX_NB_OF_HOLE],
            live_object_mark: true,
        }
    }

    pub(crate) fn alloc(&mut self, alloc_size: usize) -> Result<InBlockPtr, HeapError> {
        // linearly search for an empty slot big enough for `alloc_size`
        for block in self.blocks.iter_mut() {
            if let Some(address) = block.claim_slot(alloc_size) {
                return Ok(address);
            }
        }
        // if None was found, claim a new block of memory
        let mut block = match Block::allocate() {
            Ok(block) => block,
            Err(_) => return Err(HeapError::OOM),
        };
        // and allocate into it
        let address = match block.claim_slot(alloc_size) {
            Some(address) => address,
            None => return Err(HeapError::TooBig),
        };
        self.blocks.push(block);
        Ok(address)
    }

    #[inline(always)]
    pub(crate) fn unmarked_flag(&self) -> bool {
        !self.live_object_mark
    }

    #[inline(always)]
    pub(crate) fn marked_flag(&self) -> bool {
        self.live_object_mark
    }

    // update live object mark, so it doesn't match
    // the one use in the previous marking phase
    fn rotate_mark_flag(&mut self) {
        self.live_object_mark = !self.live_object_mark;
    }

    fn mark_blocks_for_evacuation(&mut self) {
        // build `available` histogram
        let mut free_count_by_hole_count = [0; MAX_NB_OF_HOLE];
        for block in self.blocks.iter() {
            match block.header().state {
                BlockState::PartiallyFull {
                    hole_count,
                    mark_count,
                } => {
                    free_count_by_hole_count[hole_count] += LINE_COUNT - mark_count;
                }
                _ => {}
            }
        }
        // `selected_threshold` acts as a breakpoint value, all blocks
        // above or equal to it will be selected for evacution
        let mut selected_threshold: Option<usize> = None;
        let mut available_acc = 0;
        let mut required_acc = 0;
        for hole_count in (MAX_NB_OF_HOLE - 1)..=0 {
            available_acc += free_count_by_hole_count[hole_count];
            required_acc += self.marked_lines_by_holes_count[hole_count];
            if required_acc > available_acc {
                selected_threshold = Some(hole_count);
            }
        }
        // mark blocks
        if let Some(threshold) = selected_threshold {
            for block in self.blocks.iter_mut() {
                let header = block.header_mut();
                header.state = match header.state {
                    BlockState::PartiallyFull { hole_count, .. } if hole_count >= threshold => {
                        BlockState::Evacuating
                    }
                    state => state,
                }
            }
        }
    }

    pub fn start_gc(&mut self) {
        self.mark_blocks_for_evacuation();
        // clear object marks
        for block in self.blocks.iter_mut() {
            block.clear();
        }
        self.rotate_mark_flag();
    }

    // * re compute `self.marked_lines_by_holes_count`
    // * re compute block limit
    pub fn end_gc(&mut self) {
        // clear self.marked_lines_by_holes_count
        for mark_count in self.marked_lines_by_holes_count.iter_mut() {
            *mark_count = 0;
        }
        for block in self.blocks.iter_mut() {
            block.recompute_limits();
            let block_header = block.header_mut();
            block_header.update_state();
            match block_header.state {
                BlockState::PartiallyFull {
                    hole_count,
                    mark_count,
                } => {
                    self.marked_lines_by_holes_count[hole_count] += mark_count;
                }
                _ => {}
            }
        }
    }

    /// TODO:
    /// * evacuate objects from Blocks in `Evacuating` state.
    pub fn mark_value(&mut self, value: &Value) {
        let mut object_ptrs: Vec<*const Header> = Vec::new();

        value.collect_references(&mut object_ptrs);

        let mark = self.marked_flag();
        let mut previous: Option<*const Header> = None;
        while let Some(obj_header) = object_ptrs.pop() {
            let obj_header = obj_header.cast_mut();
            unsafe {
                // skip already crawled objects
                if (*obj_header).mark == mark {
                    continue;
                }
                // mark Object, so we don't crawl it again
                (*obj_header).mark = mark;
                // get the block header so we can mark the lines containing the object
                // mark line, so we don't reclaim its memory for another object
                let block_header = BlockHeader::from_object_ptr(obj_header);
                match (*obj_header).kind {
                    Object::BoxedValue => {
                        let boxed_value = obj_header.cast::<BoxedValue>();
                        (*boxed_value).collect_references(&mut object_ptrs);
                        // mark
                        block_header.mark_lines(obj_header, (*boxed_value).size_in_bytes());
                    }
                    Object::List => {
                        let list = obj_header.cast::<List>();
                        (*list).collect_references(&mut object_ptrs);
                        block_header.mark_lines(obj_header, (*list).size_in_bytes());
                    }
                    Object::Array => {
                        // here we don't know the type of Array item,
                        // but we don't access them, so we don't care.
                        let array = obj_header.cast::<Array<()>>();
                        block_header.mark_lines(obj_header, (*array).size_in_bytes());
                    }
                    Object::Str => {
                        let string = obj_header.cast::<Str>();
                        block_header.mark_lines(obj_header, (*string).size_in_bytes());
                    }
                    Object::Tombstone => {
                        // swap references in `previous`
                        let tombstone = obj_header.cast::<Tombstone>();
                        (*tombstone).collect_references(&mut object_ptrs);
                        if let Some(previous) = previous {
                            replace_reference_in_object(
                                previous.cast_mut(),
                                obj_header,
                                (*tombstone).object_ptr,
                            );
                        }
                    }
                }
                previous = Some(obj_header);
            }
        }
    }
}

/// Replace `old_ref` by `new_ref` in the heap
/// object pointed by `object_to_update`.
unsafe fn replace_reference_in_object(
    object: *mut Header,
    old_ref: *const Header,
    new_ref: *const Header,
) {
    match (*object).kind {
        Object::BoxedValue => {
            let boxed_value = object.cast::<BoxedValue>();
            (*boxed_value).replace_reference(old_ref, new_ref);
        }
        Object::List => {
            let list = object.cast::<List>();
            (*list).replace_reference(old_ref, new_ref);
        }
        Object::Array => {
            let array = object.cast::<Array<()>>();
            (*array).replace_reference(old_ref, new_ref);
        }
        Object::Str => {
            let string = object.cast::<Str>();
            (*string).replace_reference(old_ref, new_ref);
        }
        Object::Tombstone => {
            let tombstone = object.cast::<Tombstone>();
            (*tombstone).replace_reference(old_ref, new_ref);
        }
    }
}
