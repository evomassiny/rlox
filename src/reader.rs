use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::Path;
use std::str::Chars;

/// try to parse utf-8 by batch of 128 bytes
const PARSE_BUFFER_SIZE: usize = 128;
const CHARS_BUFFER_SIZE: usize = PARSE_BUFFER_SIZE;
const BYTES_BUFFER_SIZE: usize = 1024;

#[derive(Debug)]
pub struct BufferFull;

#[derive(Debug)]
pub struct RingBuffer<const SIZE: usize, T> {
    buffer: [T; SIZE],
    start: usize,
    length: usize,
}

impl<const SIZE: usize, T: Copy> RingBuffer<SIZE, T> {
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

    /// How many elements are left in the buffer
    pub fn empty_slots(&self) -> usize {
        SIZE - self.length
    }

}

impl<const SIZE: usize> RingBuffer<SIZE, u8> {
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

#[derive(Debug)]
pub struct Utf8BufReader<T> {
    reader: T,
    bytes: RingBuffer<BYTES_BUFFER_SIZE, u8>,
    chars: RingBuffer<CHARS_BUFFER_SIZE, char>,
    parse_buffer: [u8; PARSE_BUFFER_SIZE],
}

impl <T: Read>Utf8BufReader<T> {

    pub fn parse_batch(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        // read bytes from file, to fill parse buffer
        if self.bytes.len() < self.parse_buffer.len() {
            let _ = self.bytes.fill(&mut self.reader)?;
        }
        // read at most `self.parse_buffer.len()` from the byte stream
        let mut to_parse = 0;
        for i in 0..self.parse_buffer.len() {
            match self.bytes.get(i) {
                Some(c) => {
                    self.parse_buffer[i] = c;
                    to_parse += 1;
                }
                None => break,
            }
        }
        // parse as much utf8 chars from the buffer
        match std::str::from_utf8(&self.parse_buffer[..to_parse]) {
            Ok(valid) => {
                for c in valid.chars() {
                    self.chars.append(c);
                }
                self.bytes.drop_n(to_parse);
            }
            Err(error) => {
                let count = error.valid_up_to();
                // SAFETY: safe because we know those are valid chars,
                // we just parsed them
                unsafe {
                    let valid = std::str::from_utf8_unchecked(
                        &self.parse_buffer[..count]
                    );
                    for c in valid.chars() {
                        self.chars.append(c);
                    }
                }
                self.bytes.drop_n(count);
            }
        }
        Ok(())
    }

    pub fn peek(&mut self, n: usize) -> Option<char> {
        if self.chars.len() < n {
            self.parse_batch().ok()?;
        }
        self.chars.get(n)
    }

    pub fn next(&mut self) -> Option<char> {
        if self.chars.len() == 0 {
            self.parse_batch().ok()?;
        }
        self.chars.pop()
    }

    pub fn new(reader: T) -> Self{
        Self {
            reader,
            bytes: RingBuffer::<BYTES_BUFFER_SIZE, u8>::init(0_u8),
            chars: RingBuffer::<CHARS_BUFFER_SIZE, char>::init('\0'),
            parse_buffer: [0; PARSE_BUFFER_SIZE],
        }
    }
}


#[test]
fn test_utf8_reader() {

    let path = "./test-data/valid-utf8.txt";
    let mut reader = Utf8BufReader::new(&path).expect("Failed to read test file.");
    let mut valid = String::new();
    while let Some(c) = reader.next() {
        valid.push(c);
    }
    assert_eq!(&valid, "this is valid utf8.");
}
