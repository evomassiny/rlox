use crate::heap::{Heap, HeapError};
use crate::header::Header;

const OFFSET_TO_HEADER: usize = 0; 
const OFFSET_TO_DATA: usize = 
    OFFSET_TO_HEADER
    + std::mem::size_of::<Header>()  // `header` size
    + (
        std::mem::align_of::<f64>() - (std::mem::size_of::<Header>() % std::mem::align_of::<f64>())
    ); // padding between `header` and `length`

/// data is written directly after `length`
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct Number {
    pub (crate) header: Header,
    pub data: f64,
}


impl Number {

    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: f64) -> Result<&'a mut Self, HeapError> where 'heap: 'a {
        let size: usize = OFFSET_TO_DATA + std::mem::size_of::<f64>();
        let ptr = heap.alloc(size)?;
        unsafe {
            // write `header`
            std::ptr::write(
                ptr.add(OFFSET_TO_HEADER) as *mut Header,
                Header::Number,
            );
            // write `data`
            std::ptr::write(
                ptr.add(OFFSET_TO_DATA) as *mut f64,
                value,
            );
            let obj_ref = std::mem::transmute::<*const u8, &'a mut Self>(ptr);
            Ok(obj_ref)
        }
    }

    pub fn as_mut_ref<'s>(&'s mut self) -> &'s mut f64 {
        unsafe {
            let self_ptr: *const u8 = std::mem::transmute::<&'s mut Self, *const u8>(self);
            let data_ptr = self_ptr.add(OFFSET_TO_DATA);
            std::mem::transmute::<*const u8, &'s mut f64>(data_ptr)
        }
    }
}

impl Drop for Number {
    /// leak memory, as this is handled by the GC.
    fn drop(&mut self) {
        std::mem::forget(self);
    }

}
