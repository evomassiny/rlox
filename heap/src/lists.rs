use crate::arrays::Array;
use crate::heap::Heap;
use crate::heap_objects::{Header, Markable, Object, ObjectRef};
use crate::memory::MemoryError;
use crate::values::Value;
use std::ops::{Index, IndexMut};
use std::ptr::addr_of;
use std::ptr::NonNull;

/// When the List is instanciated, its
/// base capacity is of `LIST_START_CAPACITY`.
pub(crate) const LIST_START_CAPACITY: usize = 8;

/// growable array of `Value`s
#[derive(Debug, PartialEq)]
#[repr(C)]
pub struct List {
    pub(crate) header: Header,
    /// actual amount of initialized items held
    length: usize,
    /// max amount of items *`buffer_ptr`, can hold
    capacity: usize,
    /// Exclusive ref to underlying storage
    array_ptr: *mut Array<Value>,
}

impl List {
    pub fn new<'a, 'b>(
        heap: &'a mut Heap,
    ) -> Result<&'b mut Self, MemoryError> {
        let obj_size = std::mem::size_of::<Self>();
        let ptr = heap.alloc(obj_size)?;
        let array_mut_ref = Array::<Value>::new(heap, LIST_START_CAPACITY)?;
        unsafe {
            let mut list = ptr.as_ptr().cast::<Self>();
            (*list).header = Header {
                kind: Object::List,
                mark: heap.unmarked_flag(),
            };
            (*list).length = 0;
            (*list).capacity = LIST_START_CAPACITY;
            (*list).array_ptr = array_mut_ref as *mut Array<Value>;

            Ok(&mut *list)
        }
    }

    /// replace `self.array_ptr` with an array twice as big,
    /// (copy its content in the process).
    fn grow_inner_array(&mut self, heap: &mut Heap) -> Result<(), MemoryError> {
        let new_capacity = self.capacity * 2;
        let array_mut_ref = Array::<Value>::new(heap, new_capacity)?;
        unsafe {
            // write byte array
            std::ptr::copy_nonoverlapping(
                (*self.array_ptr).buffer_ptr() as *mut u8,
                array_mut_ref.buffer_ptr() as *mut u8,
                self.capacity * std::mem::size_of::<Value>(),
            );
        }
        self.array_ptr = array_mut_ref as *mut Array<Value>;
        self.capacity = new_capacity;
        Ok(())
    }

    /// return the number of items this container currently holds.
    pub fn len(&self) -> usize {
        self.length
    }

    /// append `value` to self.
    pub fn push(
        &mut self,
        heap: &mut Heap,
        value: Value,
    ) -> Result<(), MemoryError> {
        let length = self.length;
        if self.capacity == length {
            self.grow_inner_array(heap)?;
        }
        // expect() is safe because of above check.
        unsafe {
            let item: &mut Value = (*self.array_ptr).get_mut(length);
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
            let item: Value = *(*self.array_ptr).get_mut(self.length);
            Some(item)
        }
    }
}

impl Index<usize> for List {
    type Output = Value;

    fn index(&self, index: usize) -> &Self::Output {
        if index >= self.length {
            panic!("out of bound access");
        }
        unsafe {
            // safe because of the bound check
            (*self.array_ptr).get(index)
        }
    }
}

impl IndexMut<usize> for List {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if index >= self.length {
            panic!("out of bound access");
        }
        unsafe {
            // safe because of the bound check
            (*self.array_ptr).get_mut(index)
        }
    }
}

impl Markable for List {
    /// Append array pointer + its initialize item
    /// if they contains references
    fn collect_references(&self, object_ptrs: &mut Vec<ObjectRef>) {
        /// SAFETY:
        /// safe because both &self and self.array_ptr are non Null
        unsafe {
            let array_header_ptr = addr_of!((*self.array_ptr).header);
            let self_ptr =
                Some(NonNull::new_unchecked(self as *const _ as *mut Header));
            object_ptrs.push(ObjectRef {
                origin: self_ptr,
                dest: NonNull::new_unchecked(array_header_ptr.cast_mut()),
            });
        }
        for i in 0..self.len() {
            self[i].collect_references(object_ptrs);
        }
    }

    fn size_in_bytes(&self) -> usize {
        std::mem::size_of::<Self>()
    }

    fn replace_reference(
        &mut self,
        old_ref: *const Header,
        new_ref: *const Header,
    ) {
        // replace refs in boxed values
        for i in 0..self.length {
            self[i].replace_reference(old_ref, new_ref);
        }

        // replace array
        if self.array_ptr.cast::<Header>().cast_const() == old_ref {
            self.array_ptr = new_ref.cast::<Array<Value>>().cast_mut();
        }
    }
}
