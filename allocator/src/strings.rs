use crate::heap::{Heap, HeapError};
use crate::header::Header;

const OFFSET_TO_HEADER: usize = 0; 

/// TODO:  FIX this offset
const OFFSET_TO_LENGTH: usize = 
    OFFSET_TO_HEADER
    + std::mem::size_of::<Header>()  // `header` size
    + (
        std::mem::align_of::<usize>() - (std::mem::size_of::<Header>() % std::mem::align_of::<usize>())
    ); // padding between `header` and `length`
const OFFSET_TO_BUFFER: usize = 
    OFFSET_TO_LENGTH
    + std::mem::size_of::<usize>() // `length` size
    + (
        std::mem::align_of::<u8>() - (std::mem::size_of::<usize>() % std::mem::align_of::<u8>())
    ); // padding between `length` and the first element of the char array

/// data is written directly after `length`
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct Str {
    pub (crate) header: Header,
    /// length of the string, in bytes !
    pub length: usize,
}


impl Str {

    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: &str) -> Result<&'a mut Self, HeapError> where 'heap: 'a {
        let bytes = value.as_bytes();
        let size: usize = OFFSET_TO_BUFFER + bytes.len() * std::mem::size_of::<u8>();
        let ptr = heap.alloc(size)?;
        unsafe {
            // write `header`
            std::ptr::write(
                ptr.add(OFFSET_TO_HEADER) as *mut Header,
                Header::Str,
            );
            // write `length`
            std::ptr::write(
                ptr.add(OFFSET_TO_LENGTH) as *mut usize,
                bytes.len(),
            );
            dbg!(ptr.add(OFFSET_TO_LENGTH) as *mut usize);
            dbg!(* ptr.add(OFFSET_TO_LENGTH) as *mut usize);
            // write byte array
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                ptr.add(OFFSET_TO_BUFFER) as *mut u8,
                bytes.len(),
            );
            dbg!( ptr.add(OFFSET_TO_BUFFER));
            let obj_ref = std::mem::transmute::<*const u8, &'a mut Self>(ptr);
            Ok(obj_ref)
        }
    }

    pub fn as_str<'s>(&'s self) -> &'s str {
        unsafe {
            let self_ptr = std::mem::transmute::<&'s Self, *const u8>(self);
            dbg!(self_ptr.add(OFFSET_TO_BUFFER));
            dbg!(self_ptr.add(OFFSET_TO_LENGTH));
            dbg!(self.length);
            let slice = std::slice::from_raw_parts(
                self_ptr.add(OFFSET_TO_BUFFER),
                self.length,
            );
            std::str::from_utf8_unchecked(slice)
        }
    }
}

impl Drop for Str {
    /// leak memory, as this is handled by the GC.
    fn drop(&mut self) {
        std::mem::forget(self);
    }

}

