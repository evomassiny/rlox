use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::{Header, Markable, Object};
use std::convert::AsRef;
use std::marker::{PhantomData, Sized};
use std::ptr::addr_of;

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
        let offset_to_buffer: usize = padded_offset::<Header, T>();
        let size = offset_to_buffer + array_len * std::mem::size_of::<T>();
        let ptr = heap.alloc(size)?;
        unsafe {
            let array = ptr.as_ptr().cast::<Self>();
            (*array).header = Header {
                kind: Object::Array(array_len),
                mark: false,
            };
            Ok(&mut *array)
        }
    }

    /// Pointer to the first element of the data array.
    pub(crate) fn buffer_ptr(&self) -> *const u8 {
        addr_of!(self._buffer).cast::<u8>()
    }

    /// number of element this array can hold
    pub(crate) fn size(&self) -> usize {
        let Header { kind: Object::Array(size), .. } = self.header else {
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

impl<T> Markable for Array<T> {
    /// Arrays might reference other object,
    /// but indirectly, though a List.
    /// the `Markable` impl for `List` handle it.
    fn collect_references(&self, _object_ptrs: &mut Vec<*const u8>) -> usize {
        0
    }

    /// return the size of the Header + padding + size of data buffer
    fn size_in_bytes(&self) -> usize {
        padded_offset::<Header, T>() + self.size() * std::mem::size_of::<T>()
    }
}
