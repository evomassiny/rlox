use crate::align::padded_offset;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::Header;
use crate::values::Value;
use std::convert::{AsMut, AsRef, Into};

/// Offset to start of the different fields of `BoxedValue`,
/// relative to the start of an `BoxedValue` struct.
/// (This works because of #[repr(C)])
const OFFSET_TO_HEADER: usize = 0;
const OFFSET_TO_DATA: usize = OFFSET_TO_HEADER + padded_offset::<Header, Value>();

/// A `Value` allocated in the runtime heap.
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct BoxedValue {
    pub(crate) header: Header,
    data: Value,
}

impl BoxedValue {
    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: Value) -> Result<&'a mut Self, HeapError> {
        let size: usize = std::mem::size_of::<Self>();
        let ptr = heap.alloc(size)?;
        unsafe {
            // write `header`
            std::ptr::write(ptr.add(OFFSET_TO_HEADER) as *mut Header, Header::BoxedValue);
            // write `data`
            std::ptr::write(ptr.add(OFFSET_TO_DATA) as *mut Value, value);
            let obj_ref = std::mem::transmute::<*const u8, &'a mut Self>(ptr);
            Ok(obj_ref)
        }
    }
}

impl Into<Value> for &mut BoxedValue {
    fn into(self) -> Value {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &Value>(data_ptr);
            *data_ref
        }
    }
}
impl Into<Value> for &BoxedValue {
    fn into(self) -> Value {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &Value>(data_ptr);
            *data_ref
        }
    }
}

impl AsMut<Value> for BoxedValue {
    fn as_mut(&mut self) -> &mut Value {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &mut Value>(data_ptr);
            data_ref
        }
    }
}

impl AsRef<Value> for BoxedValue {
    fn as_ref(&self) -> &Value {
        let data_ptr = &self.data as *const _ as *const u8;
        unsafe {
            let data_ref = std::mem::transmute::<*const u8, &Value>(data_ptr);
            data_ref
        }
    }
}
