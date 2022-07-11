use crate::blocks::{BlockError, BumpBlock};
use crate::objects::Allocable;

pub enum GcError {
    BufferTooSmall,
}

/// Written in each block, right before the actual Object
pub enum ObjectHeader {
    /// When an Object is evacuated (eg: relocated),
    /// we leave a Tombstone in its place, which points
    /// to the new location of the Object header.
    /// This way, while marking live objects, if some reference points
    /// to a Tombstone, we can update it to point to the new location of
    /// the object.
    Tombstone(*const u8),
    NumberObj,
}

pub struct Heap {
    blocks: Vec<BumpBlock>,
}

impl Heap {
    pub fn new() -> Self {
        Self { blocks: Vec::new() }
    }

    fn alloc(&mut self, alloc_size: usize) -> Result<*const u8, BlockError> {
        // linearly search for an empty slot big enough for `alloc_size`
        for block in self.blocks.iter_mut() {
            if let Some(address) = block.inner_alloc(alloc_size) {
                return Ok(address);
            }
        }
        // if None was found, claim a new block of memory
        let mut block = BumpBlock::new()?;
        // and allocate into it
        let address = match block.inner_alloc(alloc_size) {
            Some(address) => address,
            None => return Err(BlockError::TooBig),
        };
        self.blocks.push(block);
        Ok(address)
    }

    pub fn alloc_object<'a, 'b, T: Allocable<'b> + Default>(
        &'a mut self,
    ) -> Result<&'b mut T, BlockError>
    where
        'a: 'b,
    {
        const HEADER_SIZE: usize = std::mem::size_of::<ObjectHeader>();
        let size: usize = HEADER_SIZE + std::mem::size_of::<T>();
        let header_ptr = self.alloc(size)?;
        unsafe {
            // write header
            std::ptr::write(header_ptr as *mut ObjectHeader, T::header());
            // write object
            let object_ptr = header_ptr.add(HEADER_SIZE);
            std::ptr::write(object_ptr as *mut T, T::default());
            let obj_ref = std::mem::transmute::<*const u8, &'b mut T>(object_ptr);
            Ok(obj_ref)
        }
    }

    pub fn mark(&mut self, stack: &mut [&dyn Allocable]) {
        todo!();
    }
}
