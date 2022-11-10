use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::Header;
use std::convert::{AsMut, AsRef, Into};

/// Offset to start of the different fields of `Num`,
/// relative to the start of an `Num` struct.
/// (This works because of #[repr(C)])
const OFFSET_TO_HEADER: usize = 0;
const OFFSET_TO_DATA: usize = OFFSET_TO_HEADER + padded_offset::<Header, f64>();

/// data is written directly after `length`
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct Number {
    pub(crate) header: Header,
    data: f64,
}

impl Number {
    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: f64) -> Result<&'a mut Self, HeapError>
    where
        'heap: 'a,
    {
        let size: usize = std::mem::size_of::<Self>();
        let ptr = heap.alloc(size)?;
        unsafe {
            // write `header`
            std::ptr::write(ptr.add(OFFSET_TO_HEADER) as *mut Header, Header::Number);
            // write `data`
            std::ptr::write(ptr.add(OFFSET_TO_DATA) as *mut f64, value);
            let obj_ref = std::mem::transmute::<*const u8, &'a mut Self>(ptr);
            Ok(obj_ref)
        }
    }
}

impl Into<f64> for &mut Number {
    fn into(self) -> f64 {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &f64>(data_ptr);
            *data_ref
        }
    }
}
impl Into<f64> for &Number {
    fn into(self) -> f64 {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &f64>(data_ptr);
            *data_ref
        }
    }
}

impl AsMut<f64> for Number {
    fn as_mut(&mut self) -> &mut f64 {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &mut f64>(data_ptr);
            data_ref
        }
    }
}

impl AsRef<f64> for Number {
    fn as_ref(&self) -> &f64 {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &f64>(data_ptr);
            data_ref
        }
    }
}

impl Drop for Number {
    /// leak memory, as this is handled by the GC.
    fn drop(&mut self) {
        std::mem::forget(self);
    }
}
