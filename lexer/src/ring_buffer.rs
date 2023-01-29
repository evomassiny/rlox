use std::io::Read;

#[derive(Debug, PartialEq, Eq)]
pub struct BufferFull;

/// stack allocated buffer,
/// you can only append data to its back,
/// and pop item from the front.
#[derive(Debug)]
pub struct RingBuffer<const SIZE: usize, T> {
    buffer: [T; SIZE],
    start: usize,
    length: usize,
}

impl<const SIZE: usize, T: Copy> RingBuffer<SIZE, T> {
    /// Build an empty RingBuffer
    pub fn init(value: T) -> Self {
        Self {
            buffer: [value; SIZE],
            start: 0,
            length: 0,
        }
    }

    /// Add an item at the end of the buffer
    pub fn append(&mut self, item: T) -> Result<(), BufferFull> {
        if self.length == SIZE {
            return Err(BufferFull);
        }
        let idx = (self.start + self.length) % SIZE;
        self.buffer[idx] = item;
        self.length += 1;
        Ok(())
    }

    /// pop the first item of the buffer
    pub fn pop(&mut self) -> Option<T> {
        if self.length == 0 {
            return None;
        }
        let item = self.buffer[self.start];
        self.length -= 1;
        self.start = (self.start + 1) % SIZE;
        Some(item)
    }

    /// drop the nth first item of the buffer
    pub fn drop_n(&mut self, n: usize) {
        if self.length < n {
            panic!("Can't drop more item than wa have");
        }
        self.length -= n;
        self.start = (self.start + n) % SIZE;
    }

    /// get a copy of the index-th item in the buffer
    pub fn get(&self, index: usize) -> Option<T> {
        if index >= self.length {
            return None;
        }
        let idx = (self.start + index) % SIZE;
        Some(self.buffer[idx])
    }

    /// How many elements the buffer holds (!= capacity)
    pub fn len(&self) -> usize {
        self.length
    }

    pub fn capacity(&self) -> usize {
        SIZE
    }
}

impl<const SIZE: usize> RingBuffer<SIZE, u8> {
    /// Fill the remaining empty slots of self,
    /// by reading data from a reader.
    pub fn fill<R: Read>(&mut self, reader: &mut R) -> Result<usize, Box<dyn std::error::Error>> {
        if self.length == SIZE {
            return Ok(0);
        }
        let mut byte_read = 0;
        let end = (self.length + self.start) % SIZE;
        if self.length == 0 {
            byte_read += reader.read(&mut self.buffer[..])?;
        } else if self.start < end {
            byte_read += reader.read(&mut self.buffer[end..SIZE])?;
            byte_read += reader.read(&mut self.buffer[self.start..end])?;
        } else {
            byte_read += reader.read(&mut self.buffer[end..self.start])?;
        }
        self.length += byte_read;
        Ok(byte_read)
    }
}

#[test]
fn test_ring_push_back_pop_front() {
    const SIZE: usize = 16;
    let mut buffer: RingBuffer<SIZE, usize> = RingBuffer::init(0);

    for i in 0..SIZE {
        let _ = buffer.append(i);
    }
    assert_eq!(buffer.len(), SIZE);

    // check that the values are still there
    for i in 0..buffer.len() {
        assert_eq!(buffer.get(i), Some(i));
    }
    assert_eq!(buffer.len(), SIZE);

    // pop all content, in order
    for i in 0..SIZE {
        let item = buffer.pop();
        assert_eq!(item, Some(i));
    }
    assert_eq!(buffer.len(), 0);
}

#[test]
fn test_overfilling() {
    const SIZE: usize = 8;
    let mut buffer: RingBuffer<SIZE, usize> = RingBuffer::init(0);

    for i in 0..SIZE {
        let _ = buffer.append(i);
    }
    assert_eq!(buffer.len(), SIZE);
    assert_eq!(buffer.append(0), Err(BufferFull));
}
