use crate::lists::List;
use crate::strings::Str;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Value {
    None,
    Float(f64),
    Int(i64),
    Bool(bool),
    List(*const u8),
    Str(*const u8),
}
