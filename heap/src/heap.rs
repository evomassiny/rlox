use crate::arrays::Array;
use crate::blocks::{Block, BlockError, BlockHeader, InBlockPtr};
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

/// GC-ed Heap
pub struct Heap {
    blocks: Vec<Block>,
}

impl Heap {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn alloc(&mut self, alloc_size: usize) -> Result<InBlockPtr, HeapError> {
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

    pub fn start_gc(&mut self) {
        for block in self.blocks.iter_mut() {
            block.reset_marks();
        }
    }
    pub fn end_gc(&mut self) {
        for block in self.blocks.iter_mut() {
            block.recompute_limits();
        }
    }

    /// TODO:
    /// * evacuate objects
    /// * locate concerned block and mark it
    /// * test !
    pub fn mark_value(&mut self, value: &Value, mark: bool) {
        let mut object_ptrs: Vec<*const u8> = Vec::new();
        value.collect_references(&mut object_ptrs);

        let mut previous: Option<*const u8> = None;
        while let Some(ptr) = object_ptrs.pop() {
            unsafe {
                let obj_header: *mut Header = ptr.cast::<Header>().cast_mut();
                if (*obj_header).mark != mark {
                    // mark Object, so we don't crawl it any
                    (*obj_header).mark = mark;
                    // get the block header so we can mark the lines containing the object
                    // mark line, so we don't reclaim its memory for another object
                    let block_header: &mut BlockHeader = BlockHeader::from_object_ptr(ptr);

                    dbg!(ptr);
                    dbg!(&(*obj_header).kind);
                    match (*obj_header).kind {
                        Object::BoxedValue => {
                            let boxed_value = ptr.cast::<BoxedValue>();
                            (*boxed_value).collect_references(&mut object_ptrs);
                            // mark
                            block_header.mark_lines(ptr, (*boxed_value).size_in_bytes());
                        }
                        Object::List => {
                            let list = ptr.cast::<List>();
                            (*list).collect_references(&mut object_ptrs);
                            block_header.mark_lines(ptr, (*list).size_in_bytes());
                        }
                        Object::Array => {
                            // here we don't know the type of Array item,
                            // but we don't access them, so we don't care.
                            let array = ptr.cast::<Array<()>>();
                            block_header.mark_lines(ptr, (*array).size_in_bytes());
                        }
                        Object::Str => {
                            let string = ptr.cast::<Str>();
                            block_header.mark_lines(ptr, (*string).size_in_bytes());
                        }
                        Object::Tombstone(_new_ref) => {
                            // swap `ptr` with `new_ref` in `previous.unwrap()`
                            todo!()
                        }
                    }
                    previous = Some(ptr);
                }
            }
        }
    }
}
