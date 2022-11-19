use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::{Header, Markable, Object};
use std::convert::AsRef;
use std::marker::{PhantomData, Sized};
use std::ptr::addr_of;

/// This struct defines a Heap managed fixed size array.
/// It is not mean to be exposed to the user,
/// instead, one should use  List objects,
/// which handle re-realloaction and uninistialized memory.
///
/// `Array`s may contain weak refs to other object,
/// objects that own `Array`s are responsible for listing
/// and updating the refs in the mark/evacute phases.
///
/// NOTE: Data is written directly after `header`,
/// at `addr_of!(self._buffer)`, the size of arrays are defined
/// at runtime, so `Array` is not `Sized`.
#[derive(Debug, PartialEq)]
#[repr(C)]
pub(crate) struct Array<T> {
    /// the length of the Array is stored in the Header.
    pub(crate) header: Header,
    /// buffer size + header + full_size
    pub (crate) full_size: usize,
    /// the data itself is stored right after the header
    /// This field marks this struct as ?Sized
    _buffer: PhantomData<[T]>,
}

impl<T: Sized> Array<T> {
    pub fn new<'a, 'b>(heap: &'a mut Heap, array_len: usize) -> Result<&'b mut Self, HeapError> {
        let offset_to_buffer: usize = 
            padded_offset::<Header, usize>()    // header size + padding to `full_size`
            + padded_offset::<usize, T>();      // `full_size` size + padding to buffer
        let size = offset_to_buffer + array_len * std::mem::size_of::<T>();
        let ptr = heap.alloc(size)?;
        unsafe {
            let array = ptr.as_ptr().cast::<Self>();
            (*array).full_size = size;
            (*array).header = Header {
                kind: Object::Array,
                mark: false,
            };
            // (*array)._buffer does not need to be initialized
            Ok(&mut *array)
        }
    }

    /// Pointer to the first element of the data array.
    pub(crate) fn buffer_ptr(&self) -> *const u8 {
        addr_of!(self._buffer).cast::<u8>()
    }

    /// SAFETY:
    /// No bound checking at all !
    unsafe fn item_ptr(&self, index: usize) -> *const u8 {
         self.buffer_ptr().add(index * std::mem::size_of::<T>())
    }

    /// SAFETY: unsafe because:
    /// * the underlying item memory might be uninitialized
    /// * no bound checks
    pub(crate) unsafe fn get(&self, index: usize) -> &T {
        let item_ptr = self.item_ptr(index);
        std::mem::transmute::<*const u8, &T>(item_ptr)
    }

    /// SAFETY: unsafe because:
    /// * the underlying item memory might be uninitialized
    /// * no bound checks
    pub(crate) unsafe fn get_mut(&mut self, index: usize) -> &mut T {
        let item_ptr = self.item_ptr(index);
        std::mem::transmute::<*const u8, &mut T>(item_ptr)
    }
}

impl <T> Markable for Array<T> {
    /// Arrays might reference other object,
    /// but indirectly, though a List.
    /// the `Markable` impl for `List` handles it.
    fn collect_references(&self, _object_ptrs: &mut Vec<*const u8>) -> usize {
        // managed by the object that own the array
        0
    }

    /// return the size of the Header + padding + size of data buffer
    fn size_in_bytes(&self) -> usize {
        self.full_size
    }

    //fn replace_reference(&mut self, _old: *const u8, _new: *const u8) {
        //// managed by the object that own the array
        //return;
    //}
}
