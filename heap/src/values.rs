use crate::heap_objects::Markable;
use crate::lists::List;
use crate::strings::Str;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    Nil,
    Float(f64),
    Int(i64),
    Bool(bool),
    List(*const u8),
    Str(*const u8),
}

impl Value {
    pub(crate) fn collect_references(&self, object_ptrs: &mut Vec<*const u8>) -> usize {
        match self {
            // base value
            Value::Nil | Value::Float(_) | Value::Int(_) | Value::Bool(_) => 0,
            // append List pointer
            Value::List(ptr) => {
                object_ptrs.push(*ptr);
                1
            }
            // append Str pointer
            Value::Str(ptr) => {
                object_ptrs.push(*ptr);
                1
            }
        }
    }
}
