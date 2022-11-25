use crate::heap::{Heap, HeapError};
use crate::heap_objects::{Object, Header, Markable};

/// This object is what we replace an object with when we
/// evacuate an object.
/// NOTE: it must be the smallest object kind,
/// as it is written into the same slice as the object its replacing.
#[repr(C)]
pub (crate) struct Tombstone {
    pub header: Header,
    pub object_ptr: *const Header,
}
impl Tombstone {

    /// Allocate new Tombstone into `heap`
    pub fn new<'a, 'b>(heap: &'a mut Heap, object_ptr: *const Header) -> Result<&'b mut Self, HeapError> {
        let size = std::mem::size_of::<Self>();
        let ptr = heap.alloc(size)?;
        unsafe {
            let tombstone = ptr.as_ptr().cast::<Self>();
            (*tombstone).header = Header {
                kind: Object::Tombstone,
                mark: false,
            };
            (*tombstone).object_ptr = object_ptr;
            Ok(&mut *tombstone)
        }
    }
}

impl Markable for Tombstone {
    /// collect references objects for marking
    fn collect_references(&self, object_ptrs: &mut Vec<*const Header>) {
        object_ptrs.push(self.object_ptr);
    }

    /// returns the size of the object + its header
    fn size_in_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    /// Replace a given reference by a new one.
    /// (Usefull when "evacutation" occurs)
    fn replace_reference(&mut self, old_ref: *const Header, new_ref: *const Header) {
        // NOTE: I'm not sure about this one,
        // how a tombstone could point to another one ?
        // anyway, in such cases, I don't see why we couldn't update `self.object_ptr`
        if old_ref == self.object_ptr {
            self.object_ptr = new_ref;
        }
    }

}

