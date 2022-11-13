/// Written in each block, right before the actual Object
/// TODO: replace by a tagged pointer
#[derive(Debug, PartialEq)]
pub enum Header {
    /// When an Object is evacuated (eg: relocated),
    /// we leave a Tombstone in its place, which points
    /// to the new location of the Object header.
    /// This way, while marking live objects, if some reference points
    /// to a Tombstone, we can update it to point to the new location of
    /// the object.
    Tombstone(*const u8),
    /// A (stack) value, moved onto the heap
    BoxedValue,
    /// An immuatble char array,
    Str,
    /// A fixed size array
    Array(usize),
    /// A growable array
    List,
}
