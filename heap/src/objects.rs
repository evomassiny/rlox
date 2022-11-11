use crate::heap::{GcError, ObjectHeader};

/// Any Object that implements `Allocable`
/// can be allocated and garbage collected by the Heap
pub trait Allocable<'a> {
    fn header() -> ObjectHeader
    where
        Self: Sized;
    fn from_header(header: &'a ObjectHeader) -> &'a Self
    where
        Self: Sized;
    fn list_children(&'a self, buffer: &mut [&dyn Allocable]) -> Result<(), GcError>;
}
