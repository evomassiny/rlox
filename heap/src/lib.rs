mod align;
mod blocks;
mod heap;
mod heap_objects;
mod numbers;
mod strings;

pub use crate::heap::{Heap, HeapError};
pub use crate::numbers::Number;
pub use crate::strings::Str;

#[test]
fn test_alloc_number() {
    let mut heap = Heap::new();
    let number: &mut Number = Number::new(&mut heap, 2.0).expect("Number allocation failed");
    *number.as_mut() = 45.;
    let value: f64 = *number.as_ref();
    assert_eq!(value, 45.);
    drop(number);
}

#[test]
fn test_alloc_str() {
    let mut heap = Heap::new();
    let string: &mut Str = Str::new(&mut heap, "covefefe").expect("Str allocation failed");
    assert_eq!(string.as_ref(), "covefefe");
    drop(string);
}
