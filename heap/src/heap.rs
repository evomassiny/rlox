use crate::arrays::Array;
use crate::block_headers::{BlockHeader, BlockState};
use crate::blocks::{Block, BlockError, InBlockPtr, LINE_COUNT};
use crate::boxed_values::BoxedValue;
use crate::heap_objects::{Header, Markable, Object};
use crate::lists::List;
use crate::strings::Str;
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
    /// flag used to mark live object,
    /// we flip it during each collection
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

    // TODO!
    // compute available histogram,
    // and select Block for evacuation.
    pub fn start_gc(&mut self) {
        self.rotate_mark_flag();
        // clear object marks
        for block in self.blocks.iter_mut() {
            block.clear();
        }
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
    /// * evacuate objects
    pub fn mark_value(&mut self, value: &Value) {
        let mut object_ptrs: Vec<*const Header> = Vec::new();

        value.collect_references(&mut object_ptrs);

        let mark = self.marked_flag();
        let mut previous: Option<*const Header> = None;
        while let Some(obj_header) = object_ptrs.pop() {
            let obj_header = obj_header.cast_mut();
            unsafe {
                if (*obj_header).mark != mark {
                    // mark Object, so we don't crawl it any
                    (*obj_header).mark = mark;
                    // get the block header so we can mark the lines containing the object
                    // mark line, so we don't reclaim its memory for another object
                    let block_header: &mut BlockHeader = BlockHeader::from_object_ptr(obj_header);
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
                            // swap `ptr` with `new_ref` in `previous.unwrap()`
                            todo!()
                        }
                    }
                    previous = Some(obj_header);
                }
            }
        }
    }
}
