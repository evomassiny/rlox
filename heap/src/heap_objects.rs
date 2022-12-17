use crate::{
    arrays::Array, boxed_values::BoxedValue, compactor::LivenessFlag, lists::List, strings::Str,
    tombstones::Tombstone, values::Value,
};
use std::ptr::NonNull;

/// Written in each block, right before the actual Object
/// TODO: replace by a tagged pointer
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Object {
    /// When an Object is evacuated (eg: relocated),
    /// we leave a Tombstone in its place, which points
    /// to the new location of the Object header.
    /// This way, while marking live objects, if some reference points
    /// to a Tombstone, we can update it to point to the new location of
    /// the object.
    Tombstone,
    /// A (stack) value, moved onto the heap
    BoxedValue,
    /// An immutable char array,
    Str,
    /// A fixed size array
    Array,
    /// A growable array
    List,
}

#[derive(Debug, PartialEq)]
pub struct Header {
    /// a description of the object Type
    pub kind: Object,
    /// Mark an object as "crawled" during
    /// a GC cycle.
    pub mark: LivenessFlag,
}

impl Header {
    /// return the size of the object wrapping this header
    ///
    /// SAFETY:
    /// pattern match `self.kind` to find the correct implementation of `Markable`,
    /// only safe if:
    /// * the wrapping type layout starts with `Header` (eg: repr(C))
    /// * `self.kind` is consitent with the underlying object.
    /// Both conditions should always be true.
    pub fn object_size_in_bytes(&self) -> usize {
        let obj_header = self as *const Header;
        match self.kind {
            Object::BoxedValue => {
                let boxed_value = obj_header.cast::<BoxedValue>();
                unsafe { (*boxed_value).size_in_bytes() }
            }
            Object::List => {
                let list = obj_header.cast::<List>();
                unsafe { (*list).size_in_bytes() }
            }
            Object::Array => {
                let array = obj_header.cast::<Array<()>>();
                unsafe { (*array).size_in_bytes() }
            }
            Object::Str => {
                let string = obj_header.cast::<Str>();
                unsafe { (*string).size_in_bytes() }
            }
            Object::Tombstone => {
                let tombstone = obj_header.cast::<Tombstone>();
                unsafe { (*tombstone).size_in_bytes() }
            }
        }
    }
}

/// Defined a reference,
/// eg: the object pointed by `origin`,
/// contains a ref to `dest`.
pub struct ObjectRef {
    pub origin: Option<NonNull<Header>>,
    pub dest: NonNull<Header>,
}

pub trait Markable {
    /// append pointers to heap objects referenced by `&self,`
    /// and returns the number of "appended" refs.
    fn collect_references(&self, object_ptrs: &mut Vec<ObjectRef>);

    /// returns the size of the object + its header
    fn size_in_bytes(&self) -> usize;

    /// Replace a given reference by a new one.
    /// (Usefull when "evacutation" occurs)
    fn replace_reference(&mut self, old_ref: *const Header, new_ref: *const Header);
}
