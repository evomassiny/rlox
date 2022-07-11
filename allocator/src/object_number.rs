use crate::heap::{GcError, ObjectHeader};
use crate::objects::Allocable;

#[derive(Debug, PartialEq)]
pub struct Number(f64);
impl Number {
    pub fn new(value: f64) -> Self {
        Self(value)
    }

    pub fn set(&mut self, value: f64) {
        self.0 = value;
    }
}

impl<'a> Allocable<'a> for Number {
    fn header() -> ObjectHeader {
        ObjectHeader::NumberObj
    }

    fn from_header(header: &'a ObjectHeader) -> &'a Self {
        const HEADER_SIZE: usize = std::mem::size_of::<ObjectHeader>();
        unsafe {
            let mut obj_ptr = std::mem::transmute::<&'a ObjectHeader, *const u8>(header);
            obj_ptr = obj_ptr.add(HEADER_SIZE);
            std::mem::transmute::<*const u8, &'a Self>(obj_ptr)
        }
    }

    fn list_children(&'a self, buffer: &mut [&dyn Allocable]) -> Result<(), GcError> {
        Ok(())
    }
}

impl Drop for Number {
    /// leak memory, as this is handled by the GC.
    fn drop(&mut self) {
        std::mem::forget(self);
    }
}

impl Default for Number {
    fn default() -> Self {
        Self(0.)
    }
}
