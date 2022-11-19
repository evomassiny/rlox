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
    pub(crate) fn collect_references(&self, object_ptrs: &mut Vec<*const Header>) -> usize {
        match self {
            // base value
            Value::Nil | Value::Float(_) | Value::Int(_) | Value::Bool(_) => 0,
            // append List pointer
            Value::List(ptr) => {
                object_ptrs.push(addr_of!((*ptr).header));
                1
            }
            // append Str pointer
            Value::Str(ptr) => {
                object_ptrs.push(addr_of!((*ptr).header));
                1
            }
        }
    }
}
