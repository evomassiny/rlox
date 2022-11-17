use crate::arrays::Array;
use crate::heap::{Heap, HeapError};
use crate::heap_objects::{Header, Markable, Object};
use crate::values::Value;
use std::ops::{Index, IndexMut};
use std::ptr::addr_of;

/// When the List is instanciated, its
/// base capacity is of `LIST_START_CAPACITY`.
pub(crate) const LIST_START_CAPACITY: usize = 8;

/// growable array of `Value`s
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct List<'buf> {
    pub(crate) header: Header,
    /// actual amount of initialized items held
    length: usize,
    /// max amount of items *`buffer_ptr`, can hold
    capacity: usize,
    /// Exclusive ref to underlying storage
    array_ptr: &'buf mut Array<Value>,
}

impl<'buf> List<'buf> {
    pub fn new<'a>(heap: &'a mut Heap) -> Result<&'buf mut Self, HeapError> {
        let obj_size = std::mem::size_of::<Self>();
        let ptr = heap.alloc(obj_size)?;
        let array_mut_ref = Array::<Value>::new(heap, LIST_START_CAPACITY)?;
        unsafe {
            let mut list = ptr.as_ptr().cast::<Self>();
            (*list).header = Header {
                kind: Object::List,
                mark: false,
            };
            (*list).length = 0;
            (*list).capacity = LIST_START_CAPACITY;
            (*list).array_ptr = array_mut_ref;

            Ok(&mut *list)
        }
    }

    /// replace `self.array_ptr` with an array twice as big,
    /// (copy its content in the process).
    fn grow_inner_array(&mut self, heap: &mut Heap) -> Result<(), HeapError> {
        let array_mut_ref = Array::<Value>::new(heap, self.capacity * 2)?;
        unsafe {
            // write byte array
            std::ptr::copy_nonoverlapping(
                self.array_ptr.buffer_ptr() as *mut u8,
                array_mut_ref.buffer_ptr() as *mut u8,
                self.array_ptr.size() * std::mem::size_of::<Value>(),
            );
        }
        self.array_ptr = array_mut_ref;
        self.capacity = self.array_ptr.size();
        Ok(())
    }

    /// return the number of items this container currently holds.
    pub fn len(&self) -> usize {
        self.length
    }

    /// append `value` to self.
    pub fn push(&mut self, heap: &mut Heap, value: Value) -> Result<(), HeapError> {
        let length = self.length;
        if self.capacity == length {
            self.grow_inner_array(heap)?;
        }
        // expect() is safe because of above check.
        unsafe {
            let item: &mut Value = self
                .array_ptr
                .get_mut(length)
                .expect("The underlying array s full.");
            *item = value;
        }
        self.length += 1;
        Ok(())
    }

    /// remove last element, and return it.
    pub fn pop(&mut self) -> Option<Value> {
        if self.length == 0 {
            return None;
        }
        self.length -= 1;
        // expect() is safe because of above check.
        unsafe {
            let item: Value = *self
                .array_ptr
                .get_mut(self.length)
                .expect("The underlying array s full.");
            Some(item)
        }
    }
}

impl Index<usize> for List<'_> {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.length {
            panic!("out of bound access");
        }
        unsafe {
            // unwrap() is safe because of the bound check
            self.array_ptr.get(index).unwrap()
        }
    }
}

impl IndexMut<usize> for List<'_> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index >= self.length {
            panic!("out of bound access");
        }
        unsafe {
            // unwrap() is safe because of the bound check
            self.array_ptr.get_mut(index).unwrap()
        }
    }
}

impl Markable for List<'_> {
    /// Append array pointer + its initialize item
    /// if they contains references
    fn collect_references(&self, object_ptrs: &mut Vec<*const u8>) -> usize {
        object_ptrs.push(self.array_ptr as *const _ as *const u8);
        for i in 0..self.len() {
            // unwrap() is safe because i belonf to [0, self.len()]
            let item_ref = unsafe { self.array_ptr.get(i).unwrap() };
            object_ptrs.push(item_ref as *const _ as *const u8);
        }
        self.len() + 1
    }

    fn size_in_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}
