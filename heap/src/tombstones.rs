use crate::blocks::InBlockPtr;
use crate::compactor::LivenessFlag;
use crate::heap::Heap;
use crate::heap_objects::{Header, Markable, Object, ObjectRef};
use crate::memory::MemoryError;
use std::ptr::NonNull;

/// This object is what we replace an object with when we
/// evacuate an object.
/// NOTE: it must be the smallest object kind,
/// as it is written into the same slice as the object its replacing.
#[repr(C)]
pub(crate) struct Tombstone {
    pub header: Header,
    pub object_ptr: *const Header,
}
impl Tombstone {
    /// Write a tombstone into `old_ref`
    /// the tombstone contains a field pointing to `new_ref`.
    /// SAFETY:
    /// `old_ref` must point to a chunk allocated memory,
    /// big enough to contain a `Self`.
    pub fn replace_object_with_tombstone(
        old_ref: *const Header,
        new_ref: *const Header,
        gc_mark: LivenessFlag,
    ) {
        unsafe {
            let tombstone = old_ref.cast::<Self>().cast_mut();
            (*tombstone).header = Header {
                kind: Object::Tombstone,
                mark: gc_mark,
            };
            (*tombstone).object_ptr = new_ref;
        }
    }
}

impl Markable for Tombstone {
    /// collect references objects for marking
    fn collect_references(&self, object_ptrs: &mut Vec<ObjectRef>) {
        /// SAFETY:
        /// safe because both &self and self.object_ptr are non Null
        unsafe {
            let self_ptr = Some(NonNull::new_unchecked(self as *const _ as *mut Header));
            object_ptrs.push(ObjectRef {
                origin: self_ptr,
                dest: NonNull::new_unchecked(self.object_ptr.cast_mut()),
            });
        }
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
