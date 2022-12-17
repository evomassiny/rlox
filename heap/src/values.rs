use crate::heap_objects::{Header, Markable, ObjectRef};
use crate::lists::List;
use crate::strings::Str;
use std::ptr::{addr_of, NonNull};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Nil,
    Float(f64),
    Int(i64),
    Bool(bool),
    List(*const List),
    Str(*const Str),
}

impl Value {
    pub(crate) fn collect_references(&self, object_ptrs: &mut Vec<ObjectRef>) {
        let self_ptr = unsafe { Some(NonNull::new_unchecked(self as *const _ as *mut Header)) };
        match *self {
            // base value
            Value::Nil | Value::Float(_) | Value::Int(_) | Value::Bool(_) => {}
            // append List pointer
            Value::List(ptr) => {
                object_ptrs.push(ObjectRef {
                    origin: self_ptr,
                    dest: unsafe { NonNull::new_unchecked(ptr.cast::<Header>().cast_mut()) },
                });
            }
            // append Str pointer
            Value::Str(ptr) => {
                // SAFETY:
                // This is safe because we assert that the value pointer live as long as
                // this value
                let str_header_ptr =
                    unsafe { NonNull::new_unchecked(addr_of!((*ptr).header).cast_mut()) };
                object_ptrs.push(ObjectRef {
                    origin: self_ptr,
                    dest: str_header_ptr,
                });
            }
        }
    }

    pub(crate) fn replace_reference(&mut self, old_ref: *const Header, new_ref: *const Header) {
        *self = match *self {
            Value::Nil | Value::Float(_) | Value::Int(_) | Value::Bool(_) => *self,
            Value::List(ptr) => {
                if ptr == old_ref.cast::<List>() {
                    Value::List(new_ref.cast::<List>())
                } else {
                    Value::List(ptr)
                }
            }
            Value::Str(ptr) => {
                if ptr == old_ref.cast::<Str>() {
                    Value::Str(new_ref.cast::<Str>())
                } else {
                    Value::Str(ptr)
                }
            }
        }
    }
}
