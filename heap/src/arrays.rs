use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::Header;
use std::convert::AsRef;
use std::marker::{PhantomData, Sized};

/// Offset to start of the different fields of `Array`,
/// relative to the start of an `Array` struct.
/// (This works because of #[repr(C)])
const OFFSET_TO_HEADER: usize = 0;

/// This struct defines a Heap managed fixed size array.
/// It is not mean to be expose to the user,
/// instead, one should use  List objects,
/// which handle re-realloaction and uninistialized memory.
/// data is written directly after `header`
#[derive(Debug, PartialEq)]
#[repr(C)]
pub(crate) struct Array<T> {
    /// the length of the Array is stored in the Header.
    pub(crate) header: Header,
    /// the data itself is stored right after the header
    /// This field marks this struct as ?Sized
    _buffer: PhantomData<[T]>,
}

impl<T: Sized> Array<T> {
    pub fn new<'a, 'b>(heap: &'a mut Heap, array_len: usize) -> Result<&'b mut Self, HeapError> {
        let offset_to_buffer: usize = OFFSET_TO_HEADER + padded_offset::<Header, T>();
        let size = offset_to_buffer + array_len * std::mem::size_of::<T>();
        let ptr = heap.alloc(size)?;
        unsafe {
            // write `header`
            std::ptr::write(
                ptr.add(OFFSET_TO_HEADER) as *mut Header,
                Header::Array(array_len),
            );
            let obj_ref = std::mem::transmute::<*const u8, &'b mut Self>(ptr);
            Ok(obj_ref)
        }
    }

    /// Pointer to the first element of the data array.
    pub(crate) fn buffer_ptr(&self) -> *const u8 {
        &self._buffer as *const _ as *const u8
    }

    /// number of element this array can hold
    pub(crate) fn size(&self) -> usize {
        let Header::Array(size) = self.header else {
            panic!("Invalid access, header should be an array.");
        };
        size
    }

    fn item_ptr(&self, index: usize) -> Option<*const u8> {
        if index >= self.size() {
            return None;
        }
        unsafe { Some(self.buffer_ptr().add(index * std::mem::size_of::<T>())) }
    }

    /// SAFETY: unsafe because the underlying item memory might be uninitialized
    pub(crate) unsafe fn get(&self, index: usize) -> Option<&T> {
        let item_ptr = self.item_ptr(index)?;
        Some(std::mem::transmute::<*const u8, &T>(item_ptr))
    }

    /// SAFETY: unsafe because the underlying item memory might be uninitialized
    pub(crate) unsafe fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        let item_ptr = self.item_ptr(index)?;
        Some(std::mem::transmute::<*const u8, &mut T>(item_ptr))
    }
}
