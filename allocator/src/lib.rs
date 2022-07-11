mod blocks;
mod heap;
mod object_number;
mod objects;

use crate::blocks::{BlockError, BumpBlock};
use crate::heap::{GcError, Heap, ObjectHeader};
use crate::object_number::Number;
use crate::objects::Allocable;

#[test]
fn test_alloc_number() {
    let mut heap = Heap::new();
    let number: &mut Number = heap
        .alloc_object::<Number>()
        .expect("Number allocation failed");
    number.set(2.0);
    assert_eq!(*number, Number::new(2.));
    drop(number);
}
