use crate::heap::{Heap, HeapError};
use crate::heap_objects::{Header, Markable, Object};
use crate::values::Value;
use std::convert::{AsMut, AsRef, Into};
use std::ptr::addr_of;

/// A `Value` allocated in the runtime heap.
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct BoxedValue {
    pub(crate) header: Header,
    value: Value,
}

impl BoxedValue {
    pub fn new<'heap, 'a>(heap: &'heap mut Heap, value: Value) -> Result<&'a mut Self, HeapError> {
        let size: usize = std::mem::size_of::<Self>();
        let ptr = heap.alloc(size)?;
        unsafe {
            let mut boxed_value = ptr.as_ptr().cast::<Self>();
            (*boxed_value).header = Header {
                kind: Object::BoxedValue,
                mark: false,
            };
            (*boxed_value).value = value;
            Ok(&mut *boxed_value)
        }
    }
}

impl Into<Value> for &mut BoxedValue {
    fn into(self) -> Value {
        self.value
    }
}
impl Into<Value> for &BoxedValue {
    fn into(self) -> Value {
        self.value
    }
}

impl AsMut<Value> for BoxedValue {
    fn as_mut(&mut self) -> &mut Value {
        &mut self.value
    }
}

impl AsRef<Value> for BoxedValue {
    fn as_ref(&self) -> &Value {
        &self.value
    }
}

impl Markable for BoxedValue {
    /// Arrays might reference other object,
    /// but indirectly, though a List.
    /// the `Markable` impl for `List` handle it.
    fn collect_references(&self, object_ptrs: &mut Vec<*const Header>) {
        self.value.collect_references(object_ptrs);
    }

    fn size_in_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}