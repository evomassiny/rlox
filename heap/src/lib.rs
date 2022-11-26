mod align;
mod arrays;
mod block_headers;
mod blocks;
mod boxed_values;
mod heap;
mod heap_objects;
mod lists;
mod strings;
mod tombstones;
mod values;

use crate::arrays::Array;
pub use crate::boxed_values::BoxedValue;
pub use crate::heap::{Heap, HeapError};
pub use crate::lists::List;
pub use crate::strings::Str;
use crate::tombstones::Tombstone;
pub use crate::values::Value;

use crate::lists::LIST_START_CAPACITY;

#[test]
fn test_alloc_boxed_values() {
    let mut heap = Heap::new();
    let number: &mut BoxedValue =
        BoxedValue::new(&mut heap, Value::Int(1)).expect("BoxedValue allocation failed");
    *number.as_mut() = Value::Int(4);
    let value: Value = *number.as_ref();
    assert_eq!(value, Value::Int(4));
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
        *array.get_mut(0) = 45;
        assert_eq!(*array.get(0), 45);

        *array.get_mut(1) = 666;
        assert_eq!(*array.get(1), 666);
        assert_eq!(*array.get(0), 45);

        *array.get_mut(0) = 555;
        assert_eq!(*array.get(0), 555);
        assert_eq!(*array.get(1), 666);
    };
}

#[test]
fn test_alloc_list() {
    let mut heap = Heap::new();
    let list: &mut List = List::new(&mut heap).expect("list allocation failed");
    assert_eq!(list.len(), 0);
    list.push(&mut heap, Value::Bool(true));
    assert_eq!(list.len(), 1);
    assert_eq!(list[0], Value::Bool(true));
}

#[test]
fn test_realloc_list() {
    let mut heap = Heap::new();
    let list: &mut List = List::new(&mut heap).expect("list allocation failed");
    // push more element that the original buffer can hold,
    // to trigger a reallocation
    for i in 0..(LIST_START_CAPACITY * 2) {
        list.push(&mut heap, Value::Int(i as i64));
    }
    assert_eq!(list.len(), LIST_START_CAPACITY * 2);
    for i in 0..(LIST_START_CAPACITY * 2) {
        assert_eq!(list[i], Value::Int(i as i64));
    }
}

#[test]
fn test_item_mutability() {
    let mut heap = Heap::new();
    let list: &mut List = List::new(&mut heap).expect("list allocation failed");
    list.push(&mut heap, Value::Bool(true));
    assert_eq!(list[0], Value::Bool(true));
    list[0] = Value::Bool(false);
    assert_eq!(list[0], Value::Bool(false));
}

#[test]
#[should_panic]
fn test_out_of_bound_access_list() {
    let mut heap = Heap::new();
    let list: &mut List = List::new(&mut heap).expect("list allocation failed");
    list.push(&mut heap, Value::Bool(true));
    list[2] = Value::Bool(false); // out of bound
}

#[test]
fn test_marking() {
    use std::ptr::addr_of;
    let mut heap = Heap::new();
    let str_a: &mut Str = Str::new(&mut heap, "aaaa").expect("Str allocation failed");
    let str_b: &mut Str = Str::new(&mut heap, "dddd").expect("Str allocation failed");
    drop(str_b);
    let str_c: &mut Str = Str::new(&mut heap, "cccc").expect("Str allocation failed");

    let list: &mut List = List::new(&mut heap).expect("list allocation failed");
    list.push(&mut heap, Value::Str(str_a as *const Str));
    list.push(&mut heap, Value::Str(str_a as *const Str));
    let list_value = Value::List(addr_of!(*list) as *const List);

    heap.start_gc();
    heap.mark_value(&list_value);
    heap.end_gc();

    assert_eq!(list.len(), 2);
}
