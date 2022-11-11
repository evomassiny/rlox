use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::Header;
use std::convert::AsRef;

/// Offset to start of the different fields of `Str`,
/// relative to the start of an `Str` struct.
/// (This works because of #[repr(C)])
const OFFSET_TO_HEADER: usize = 0;
const OFFSET_TO_LENGTH: usize = OFFSET_TO_HEADER + padded_offset::<Header, usize>();
const OFFSET_TO_BUFFER: usize = OFFSET_TO_LENGTH + padded_offset::<usize, u8>();

/// data is written directly after `length`
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct Str {
    pub(crate) header: Header,
    /// length of the string, in bytes !
    length: usize,
}

impl Str {
    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: &str) -> Result<&'a mut Self, HeapError>
    where
        'heap: 'a,
    {
        let bytes = value.as_bytes();
        let size: usize = OFFSET_TO_BUFFER + bytes.len() * std::mem::size_of::<u8>();
        let ptr = heap.alloc(size)?;
        unsafe {
            // write `header`
            std::ptr::write(ptr.add(OFFSET_TO_HEADER) as *mut Header, Header::Str);
            // write `length`
            std::ptr::write(ptr.add(OFFSET_TO_LENGTH) as *mut usize, bytes.len());
            // write byte array
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                ptr.add(OFFSET_TO_BUFFER) as *mut u8,
                bytes.len(),
            );
            let obj_ref = std::mem::transmute::<*const u8, &'a mut Self>(ptr);
            Ok(obj_ref)
        }
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        let self_ptr = self as *const _ as *const u8;
        unsafe {
            let slice = std::slice::from_raw_parts(self_ptr.add(OFFSET_TO_BUFFER), self.length);
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
