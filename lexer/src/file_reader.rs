use super::ring_buffer::RingBuffer;
use super::source::{PeekOffset, ReadError};

use std::io::prelude::*;

/// try to parse utf-8 by batch of 128 bytes
const PARSE_BUFFER_SIZE: usize = 128;
const CHARS_BUFFER_SIZE: usize = PARSE_BUFFER_SIZE;
const BYTES_BUFFER_SIZE: usize = 1024;

#[derive(Debug)]
pub struct ReaderPeeker<T> {
    reader: T,
    // input read buffer
    bytes: RingBuffer<BYTES_BUFFER_SIZE, u8>,
    // outout buffer
    chars: RingBuffer<CHARS_BUFFER_SIZE, char>,
    // scratch array to parse utf-8 chars
    parse_buffer: [u8; PARSE_BUFFER_SIZE],
}

impl<T: Read> ReaderPeeker<T> {
    /// parse as must chars from self.reader
    /// until either `self.chars` if full or we emptied the reader.
    pub fn parse_batch(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        // copy reader -> self.bytes
        //                self.bytes -> self.parse_buffer
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
                    let _ = self.chars.append(c);
                }
                self.bytes.drop_n(to_parse);
            }
            Err(error) => {
                let count = error.valid_up_to();
                // SAFETY: safe because we know those are valid chars,
                // we just parsed them
                unsafe {
                    let valid = std::str::from_utf8_unchecked(
                        &self.parse_buffer[..count],
                    );
                    for c in valid.chars() {
                        let _ = self.chars.append(c);
                    }
                }
                self.bytes.drop_n(count);
            }
        }
        Ok(())
    }

    pub fn new(reader: T) -> Self {
        Self {
            reader,
            bytes: RingBuffer::<BYTES_BUFFER_SIZE, u8>::init(0_u8),
            chars: RingBuffer::<CHARS_BUFFER_SIZE, char>::init('\0'),
            parse_buffer: [0; PARSE_BUFFER_SIZE],
        }
    }
}

impl<T: Read> PeekOffset for ReaderPeeker<T> {
    fn peek_at(&mut self, n: usize) -> Result<Option<char>, ReadError> {
        if n >= self.chars.capacity() {
            return Err(ReadError::PeekTooFar);
        }
        if n >= self.chars.len() {
            let res = self.parse_batch();
            if res.is_err() {
                return Err(ReadError::IoError);
            }
        }
        Ok(self.chars.get(n))
    }

    fn next(&mut self) -> Option<char> {
        if self.chars.len() == 0 {
            self.parse_batch().ok()?;
        }
        self.chars.pop()
    }

    fn capacity(&self) -> usize {
        self.chars.capacity()
    }
}

#[test]
fn test_utf8_reader() {
    use std::fs::File;
    use std::path::PathBuf;

    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("test-data/valid-utf8.txt");

    let file = File::open(path).expect("Failed to read test file.");
    let mut reader = ReaderPeeker::new(file);
    let mut valid = String::new();
    while let Some(c) = reader.next() {
        valid.push(c);
    }
    assert_eq!(&valid, "this is valid utf8.\n");
}
