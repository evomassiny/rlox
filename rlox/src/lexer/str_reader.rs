use super::ring_buffer::RingBuffer;
use super::source::{PeekOffset, ReadError};
use std::str::Chars;

pub struct StrPeeker<'src, const SIZE: usize> {
    src: Chars<'src>,
    buffer: RingBuffer<SIZE, char>,
}

impl<'src, const SIZE: usize> StrPeeker<'src, { SIZE }> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src: src.chars(),
            buffer: RingBuffer::<SIZE, char>::init('\0'),
        }
    }
}

impl<'src, const SIZE: usize> PeekOffset for StrPeeker<'src, { SIZE }> {
    fn peek_at(&mut self, n: usize) -> Result<Option<char>, ReadError> {
        if n > self.buffer.len() {
            return Err(ReadError::PeekTooFar);
        }
        while self.buffer.len() < (n + 1) {
            match self.src.next() {
                None => return Ok(None),
                Some(c) => {
                    // safe to ignore "BufferFull" errors
                    // because of previous check
                    let _ = self.buffer.append(c);
                }
            }
        }
        Ok(self.buffer.get(n))
    }

    fn next(&mut self) -> Option<char> {
        if self.buffer.len() != 0 {
            return self.buffer.pop();
        }
        self.src.next()
    }

    fn capacity(&self) -> usize {
        SIZE
    }
}
