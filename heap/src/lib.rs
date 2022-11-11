mod align;
mod arrays;
mod blocks;
mod heap;
mod heap_objects;
mod numbers;
mod strings;

use crate::arrays::Array;
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
}

#[test]
fn test_alloc_str() {
    let mut heap = Heap::new();
    let string: &mut Str = Str::new(&mut heap, "covefefe").expect("Str allocation failed");
    assert_eq!(string.as_ref(), "covefefe");
}

#[test]
fn test_alloc_array() {
    let mut heap = Heap::new();
    let array: &mut Array<usize> = Array::new(&mut heap, 10).expect("array allocation failed");
    unsafe {
        *array.get_mut(0).unwrap() = 45;
        assert_eq!(*array.get(0).unwrap(), 45);

        *array.get_mut(1).unwrap() = 666;
        assert_eq!(*array.get(1).unwrap(), 666);
        assert_eq!(*array.get(0).unwrap(), 45);

        *array.get_mut(0).unwrap() = 555;
        assert_eq!(*array.get(0).unwrap(), 555);
        assert_eq!(*array.get(1).unwrap(), 666);

        assert_eq!(array.get(10), None);
    };
}
