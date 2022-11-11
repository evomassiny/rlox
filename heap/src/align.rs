/// In struct, with a #[repr(C)] layout,
/// containing 2 Types: A followed by B,
/// returns the offset of B relative to the start or B.
///
/// # Example:
/// With size_of::<A>() == 9, and align_of::<B>() == 4,
/// we can expect 3 bytes of padding after A, as such:
/// +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+-----
/// |A |A |A |A |A |A |A |A |A |  |  |  |B |B |B |B |  |  |   
/// +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--
/// 0           4           8           12          16
/// ^                                   ^
///  \_ start of A                       \_ start of B
///
/// In this configuration, padded_offset::<A, B>() returns 12.
pub const fn padded_offset<A, B>() -> usize {
    let a_size = std::mem::size_of::<A>();
    let b_align = std::mem::align_of::<B>();

    let padding = if a_size % b_align == 0 {
        0
    } else {
        b_align - a_size % b_align
    };
    a_size + padding
}
