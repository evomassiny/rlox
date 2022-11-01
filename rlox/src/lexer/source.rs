use super::file_reader::ReaderPeeker;
use super::str_reader::StrPeeker;
use std::collections::VecDeque;
use std::fs::File;
use std::path::Path;

pub enum ReadError {
    PeekTooFar,
    IoError,
}

pub trait PeekOffset {
    fn next(&mut self) -> Option<char>;
    fn peek_at(&mut self, index: usize) -> Result<Option<char>, ReadError>;
    fn capacity(&self) -> usize;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub char_index: usize,
}

#[derive(Debug)]
pub struct SourceInput<T> {
    src: T,
    line: usize,
    column: usize,
    char_index: usize,
    growable_buffer: VecDeque<Option<char>>,
}

impl<T: PeekOffset> SourceInput<T> {
    /// return the span of the last returned
    /// char.
    pub fn span(&self) -> Span {
        Span {
            line: self.line,
            column: self.column - 1,
            char_index: self.char_index - 1,
        }
    }

    pub fn new_line(&mut self) {
        self.column = 1;
        self.line += 1;
    }

    /// read a char and update position
    pub fn advance(&mut self) -> Option<char> {
        let maybe_char: Option<char> = match self.growable_buffer.pop_front() {
            Some(maybe_char) => maybe_char,
            None => self.src.next(),
        };
        if let Some(c) = maybe_char {
            self.column += 1;
            self.char_index += 1;
            return Some(c);
        }
        None
    }

    pub fn peek(&mut self, index: usize) -> Option<char> {
        if let Some(&c) = self.growable_buffer.get(index) {
            return c;
        }
        // empty stack buffer into heap one,
        // until `index`
        while index - self.growable_buffer.len() >= self.src.capacity() {
            self.growable_buffer.push_back(self.src.next());
        }
        match self.src.peek_at(index - self.growable_buffer.len()) {
            Ok(maybe_c) => maybe_c,
            Err(ReadError::PeekTooFar) => unreachable!("check for buffer capacity did not work ?"),
            Err(ReadError::IoError) => {
                panic!("IO error while reading input. can't recover from that.")
            }
        }
    }
}

impl SourceInput<ReaderPeeker<File>> {
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Box<dyn std::error::Error>> {
        let file = File::open(path)?;
        let reader = ReaderPeeker::new(file);
        Ok(Self {
            line: 1,
            column: 1,
            char_index: 0,
            src: reader,
            growable_buffer: VecDeque::new(),
        })
    }
}

impl<'src, const SIZE: usize> SourceInput<StrPeeker<'src, { SIZE }>> {
    pub fn from_str(src: &'src str) -> Self {
        let src = StrPeeker::<'src, SIZE>::new(src);
        Self {
            line: 1,
            column: 1,
            char_index: 0,
            src,
            growable_buffer: VecDeque::new(),
        }
    }
}

#[test]
fn test_cursor() {
    let mut cursor: SourceInput<StrPeeker<'_, 64>> = SourceInput::from_str("01");
    assert_eq!(cursor.peek(0), Some('0'));
    assert_eq!(cursor.peek(1), Some('1'));
    assert_eq!(cursor.peek(2), None);
    assert_eq!(cursor.advance(), Some('0'));
    assert_eq!(cursor.advance(), Some('1'));
    assert_eq!(cursor.peek(0), None);
    assert_eq!(cursor.advance(), None);
}
