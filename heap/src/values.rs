use crate::heap_objects::{Header, Markable};
use crate::lists::List;
use crate::strings::Str;
use std::ptr::addr_of;

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
    pub(crate) fn collect_references(&self, object_ptrs: &mut Vec<*const Header>) {
        match *self {
            // base value
            Value::Nil | Value::Float(_) | Value::Int(_) | Value::Bool(_) => {},
            // append List pointer
            Value::List(ptr) => {
                // SAFETY:
                // This is safe because we assert that the value pointer live as long as 
                // this value
                
                unsafe { dbg!(&*ptr); }; // HERE (*ptr).header has already been zeroed
                object_ptrs.push(ptr.cast::<Header>());
                unsafe { dbg!(&*ptr); };
            }
            // append Str pointer
            Value::Str(ptr) => {
                // SAFETY:
                // This is safe because we assert that the value pointer live as long as 
                // this value
                let str_header_ptr = unsafe { addr_of!((*ptr).header) }; 
                object_ptrs.push(str_header_ptr);
            }
        }
    }
}
