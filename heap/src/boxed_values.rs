use crate::heap::Heap;
use crate::heap_objects::{Header, Markable, Object, ObjectRef};
use crate::memory::MemoryError;
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
    pub fn new<'heap, 'a>(
        heap: &'heap mut Heap,
        value: Value,
    ) -> Result<&'a mut Self, MemoryError> {
        let size: usize = std::mem::size_of::<Self>();
        let ptr = heap.alloc(size)?;
        unsafe {
            let mut boxed_value = ptr.as_ptr().cast::<Self>();
            (*boxed_value).header = Header {
                kind: Object::BoxedValue,
                mark: heap.unmarked_flag(),
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
    fn collect_references(&self, object_ptrs: &mut Vec<ObjectRef>) {
        self.value.collect_references(object_ptrs);
    }

    fn size_in_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn replace_reference(&mut self, old_ref: *const Header, new_ref: *const Header) {
        self.value.replace_reference(old_ref, new_ref);
    }
}
