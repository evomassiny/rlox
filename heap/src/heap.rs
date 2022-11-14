use crate::arrays::Array;
use crate::blocks::{BlockError, BumpBlock};
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
    blocks: Vec<BumpBlock>,
}

impl Heap {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    pub fn alloc(&mut self, alloc_size: usize) -> Result<*const u8, HeapError> {
        // linearly search for an empty slot big enough for `alloc_size`
        for block in self.blocks.iter_mut() {
            if let Some(address) = block.inner_alloc(alloc_size) {
                return Ok(address);
            }
        }
        // if None was found, claim a new block of memory
        let mut block = match BumpBlock::new() {
            Ok(block) => block,
            Err(_) => return Err(HeapError::OOM),
        };
        // and allocate into it
        let address = match block.inner_alloc(alloc_size) {
            Some(address) => address,
            None => return Err(HeapError::TooBig),
        };
        self.blocks.push(block);
        Ok(address)
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
                let header: &mut Header = std::mem::transmute::<*const u8, &mut Header>(ptr);
                if header.mark != mark {
                    header.mark = mark;
                    match header.kind {
                        Object::Tombstone(new_ref) => {
                            // swap `ptr` with `new_ref` in `previous.unwrap()`
                            todo!()
                        }
                        Object::BoxedValue => {
                            let boxed_value: &BoxedValue =
                                std::mem::transmute::<*const u8, &BoxedValue>(ptr);
                            boxed_value.collect_references(&mut object_ptrs);
                        }
                        Object::List => {
                            let list: &List = std::mem::transmute::<*const u8, &List>(ptr);
                            list.collect_references(&mut object_ptrs);
                        }
                        // none of those object contains strong refs
                        Object::Array(_) | Object::Str => {}
                    }
                    previous = Some(ptr);
                }
            }
        }
    }
}
