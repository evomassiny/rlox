mod blocks;

use blocks::{BlockError, BumpBlock};

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

/// Any Object that implements `Allocable`
/// can be allocated and garbage collected by the Heap
pub trait Allocable<'a> {
    fn header() -> ObjectHeader;
    fn from_header(header: &'a ObjectHeader) -> &'a Self;
    //fn iter_childrens(&'a self) -> Iterator<&'a Allocable>;
}

#[derive(Debug, PartialEq)]
pub struct Number(f64);
impl<'a> Allocable<'a> for Number {
    fn header() -> ObjectHeader {
        ObjectHeader::NumberObj
    }

    fn from_header(header: &'a ObjectHeader) -> &'a Self {
        const HEADER_SIZE: usize = std::mem::size_of::<ObjectHeader>();
        unsafe {
            let mut obj_ptr = std::mem::transmute::<&'a ObjectHeader, *const u8>(header);
            obj_ptr = obj_ptr.add(HEADER_SIZE);
            std::mem::transmute::<*const u8, &'a Self>(obj_ptr)
        }
    }
}

impl Default for Number {
    fn default() -> Self {
        Self(0.)
    }
}

#[test]
fn test_alloc_number() {
    let mut heap = Heap::new();
    let mut number: &mut Number = heap
        .alloc_object::<Number>()
        .expect("Number allocation failed");
    number.0 = 2.;
    assert_eq!(*number, Number(2.));
}

