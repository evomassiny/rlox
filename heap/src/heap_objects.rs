/// Written in each block, right before the actual Object
/// TODO: replace by a tagged pointer
#[derive(Debug, PartialEq)]
pub enum Object {
    /// When an Object is evacuated (eg: relocated),
    /// we leave a Tombstone in its place, which points
    /// to the new location of the Object header.
    /// This way, while marking live objects, if some reference points
    /// to a Tombstone, we can update it to point to the new location of
    /// the object.
    Tombstone(*const u8),
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
    pub kind: Object,
    pub mark: bool,
}

pub trait Markable {
    /// append pointers to heap objects referenced by `&self,`
    /// and returns the number of "appended" refs.
    fn collect_references(&self, object_ptrs: &mut Vec<*const Header>) -> usize;

    /// returns the size of the object + its header
    fn size_in_bytes(&self) -> usize;

    // Replace a given reference by a new one.
    // (Usefull when "evacutation" occurs)
    //fn replace_reference(&mut self, old_ref: *const u8, new_ref: *const u8);
}
