use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::{Header, Markable, Object};
use std::convert::AsRef;
use std::marker::PhantomData;
use std::ptr::{addr_of, addr_of_mut};

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
    /// the data itself is stored right after the length
    /// This field marks this struct as ?Sized
    _buffer: PhantomData<str>,
}

impl Str {
    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: &str) -> Result<&'a mut Self, HeapError> {
        let bytes = value.as_bytes();
        let size: usize = OFFSET_TO_BUFFER + bytes.len() * std::mem::size_of::<u8>();
        let ptr = heap.alloc(size)?;
        unsafe {
            let mut string = ptr.as_ptr().cast::<Self>();
            (*string).header = Header {
                kind: Object::Str,
                mark: false,
            };
            (*string).length = bytes.len();
            // write byte array
            std::ptr::copy_nonoverlapping(
                bytes.as_ptr(),
                addr_of_mut!((*string)._buffer).cast::<u8>(),
                bytes.len(),
            );
            Ok(&mut *string)
        }
    }
}

impl AsRef<str> for Str {
    fn as_ref(&self) -> &str {
        let buffer_ptr = addr_of!(self._buffer).cast::<u8>();
        unsafe {
            let slice = std::slice::from_raw_parts(buffer_ptr, self.length);
            std::str::from_utf8_unchecked(slice)
        }
    }
}

impl Markable for Str {
    /// Str do not reference any other data
    fn collect_references(&self, _object_ptrs: &mut Vec<*const Header>) -> usize {
        0
    }

    fn size_in_bytes(&self) -> usize {
        // OFFSET_TO_BUFFER includes padding
        return OFFSET_TO_BUFFER + self.length;
    }
}
