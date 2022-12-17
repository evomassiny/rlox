use crate::{
    arrays::Array,
    block_headers::{BlockHeader, BlockState},
    blocks::{Block, LINE_COUNT},
    boxed_values::BoxedValue,
    heap_objects::{Header, Markable, Object, ObjectRef},
    lists::List,
    memory::{Memory, MemoryError, MAX_NB_OF_HOLE_IN_BLOCK},
    strings::Str,
    tombstones::Tombstone,
    values::Value,
};
use std::ptr::NonNull;

pub type LivenessFlag = bool;

pub struct HeapCompactor {
    /// total number of marked lines (accross all blocks),
    /// indexed by the nb of hole in their respective block.
    /// (used in the evacutation selection heuristic)
    marked_lines_by_holes_count: [usize; MAX_NB_OF_HOLE_IN_BLOCK],

    /// flag used to mark live object,
    /// we flip it before each gc cycle.
    live_object_flag: LivenessFlag,
}

impl HeapCompactor {
    pub fn new() -> Self {
        Self {
            marked_lines_by_holes_count: [0; MAX_NB_OF_HOLE_IN_BLOCK],
            live_object_flag: true,
        }
    }

    /// Mark fragmented blocks for "evacuation".
    fn mark_blocks_for_evacuation(&mut self, memory: &mut Memory) {
        // Pick a number of holes per block,
        // all blocks containing at least that much holes will be evacuated
        // into the blocks with less holes.
        //
        // We pick the minimum `hole count` where objects located within blocks
        // above that threshold can still fit in in the blocks below it.
        let mut selected_threshold: Option<usize> = None;

        // Group blocks by hole count, and sum the number of free slots.
        let mut free_count_by_hole_count: [usize; MAX_NB_OF_HOLE_IN_BLOCK] =
            memory.compute_availabily_histogram();

        // compute the whole number of free slots
        let mut cumulative_free_count = 0;
        for free_count in free_count_by_hole_count {
            cumulative_free_count += free_count;
        }

        // pick the minimum threshold value where
        // all blocks above it can still fit in the blocks below it.
        let mut cumulative_full_count = 0;
        for thresold_cursor in (MAX_NB_OF_HOLE_IN_BLOCK - 1)..=0 {
            // free lines in block _below_ `threshold_cursor`.
            cumulative_free_count -= free_count_by_hole_count[thresold_cursor];
            // full lines in block _over_ `threshold_cursor`.
            cumulative_full_count += self.marked_lines_by_holes_count[thresold_cursor];

            // stop if the evacuated block data cannot fit in
            // the rest of blocks.
            if cumulative_full_count > cumulative_free_count {
                selected_threshold = Some(thresold_cursor + 1);
                break;
            }
        }

        // mark blocks with a hole count greater or equal than the threshold.
        if let Some(threshold) = selected_threshold {
            for block in memory.blocks.iter_mut() {
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

    // update live object mark, so it doesn't match
    // the one used in the previous marking phase.
    fn update_cycle_flag(&mut self) {
        self.live_object_flag = !self.live_object_flag;
    }

    pub fn init_cycle(&mut self, memory: &mut Memory) {
        self.mark_blocks_for_evacuation(memory);
        // clear live lines marks
        for block in memory.blocks.iter_mut() {
            block.clear();
        }
        self.update_cycle_flag();
    }

    /// TODO:
    /// * evacuate objects from Blocks in `Evacuating` state.
    pub fn mark_and_compact(&self, value: &Value, memory: &mut Memory) -> Result<(), MemoryError> {
        let mut refs: Vec<ObjectRef> = Vec::new();

        value.collect_references(&mut refs);

        let mark = self.live_object_flag;
        while let Some(ObjectRef { origin, dest }) = refs.pop() {
            let mut obj_header = dest.as_ptr();
            unsafe {
                // skip already crawled objects
                if (*obj_header).mark == mark {
                    continue;
                }

                // get the block header so we can mark the lines containing the object
                // mark line, so we don't reclaim its memory for another object
                let mut block_header = BlockHeader::from_object_ptr(obj_header);

                // if the block containing the object is marked as "Evacuating",
                // move the object into a new block.
                if matches!(block_header.state, BlockState::Evacuating) {
                    let new_location = memory.evacuate((*obj_header).object_size_in_bytes())?;
                    // copy old object to new location
                    std::ptr::copy_nonoverlapping(
                        obj_header.cast::<u8>(),
                        new_location.as_ptr().cast::<u8>(),
                        (*obj_header).object_size_in_bytes(),
                    );
                    // replace object with Tombstone pointing to the new ref
                    Tombstone::replace_object_with_tombstone(
                        obj_header,
                        new_location.as_ptr().cast::<Header>(),
                        mark,
                    );
                    // continue with the new ref
                    obj_header = new_location.as_ptr().cast::<Header>();
                    block_header = BlockHeader::from_object_ptr(obj_header);
                }

                // mark Object, so we don't crawl it again
                (*obj_header).mark = mark;

                match (*obj_header).kind {
                    Object::BoxedValue => {
                        let boxed_value = obj_header.cast::<BoxedValue>();
                        (*boxed_value).collect_references(&mut refs);
                        // mark
                        block_header.mark_lines(obj_header, (*boxed_value).size_in_bytes());
                    }
                    Object::List => {
                        let list = obj_header.cast::<List>();
                        (*list).collect_references(&mut refs);
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
                        // swap references in `origin`
                        let tombstone = obj_header.cast::<Tombstone>();
                        (*tombstone).collect_references(&mut refs);
                        if let Some(origin) = origin {
                            replace_reference_in_object(
                                origin.as_ptr(),
                                obj_header,
                                (*tombstone).object_ptr,
                            );
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// End a GC cycle.
    /// * re compute `self.marked_lines_by_holes_count`
    /// * update the "hole cursor" of each blocks.
    pub fn end_cycle(&mut self, memory: &mut Memory) {
        // clear self.marked_lines_by_holes_count
        for mark_count in self.marked_lines_by_holes_count.iter_mut() {
            *mark_count = 0;
        }
        for block in memory.blocks.iter_mut() {
            block.update_hole_cursor();
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

    pub fn unmarked_flag(&self) -> LivenessFlag {
        !self.live_object_flag
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
