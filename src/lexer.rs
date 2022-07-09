use std::str::Chars;

pub const BASE_PEEK_BUFFER_SIZE: usize = 8;

/// All Token variants accepted by the scanner
#[derive(Debug, PartialEq)]
pub enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // one or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // litterals
    Identifier(String),
    Str(String),
    Number(f64),
    // keywords,
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Eof,
}
impl TokenKind {
    /// handcrafted DFA
    fn parse_string(identifier: String) -> Self {
        let mut id = identifier.chars();
        match id.next() {
            // 'and'
            Some('a') => {
                if id.next() == Some('n') && id.next() == Some('d') {
                    return Self::And;
                }
            }
            // 'class'
            Some('c') => {
                if id.next() == Some('l')
                    && id.next() == Some('a')
                    && id.next() == Some('s')
                    && id.next() == Some('s')
                {
                    return Self::Class;
                }
            }
            // 'else'
            Some('e') => {
                if id.next() == Some('l') && id.next() == Some('s') && id.next() == Some('e') {
                    return Self::Else;
                }
            }
            // 'for' | 'fun' | 'false'
            Some('f') => match id.next() {
                Some('o') => {
                    if id.next() == Some('r') {
                        return Self::For;
                    }
                }
                Some('u') => {
                    if id.next() == Some('n') {
                        return Self::Fun;
                    }
                }
                Some('a') => {
                    if id.next() == Some('l') && id.next() == Some('s') && id.next() == Some('e') {
                        return Self::False;
                    }
                }
                _ => return Self::Identifier(identifier),
            },
            // 'if'
            Some('i') => {
                if id.next() == Some('f') {
                    return Self::If;
                }
            }
            // 'nil'
            Some('n') => {
                if id.next() == Some('i') && id.next() == Some('l') {
                    return Self::Nil;
                }
            }
            // 'or'
            Some('o') => {
                if id.next() == Some('r') {
                    return Self::Or;
                }
            }
            // 'print'
            Some('p') => {
                if id.next() == Some('r')
                    && id.next() == Some('i')
                    && id.next() == Some('n')
                    && id.next() == Some('t')
                {
                    return Self::Print;
                }
            }
            // 'return'
            Some('r') => {
                if id.next() == Some('e')
                    && id.next() == Some('t')
                    && id.next() == Some('u')
                    && id.next() == Some('r')
                    && id.next() == Some('n')
                {
                    return Self::Return;
                }
            }
            // 'super'
            Some('s') => {
                if id.next() == Some('u')
                    && id.next() == Some('p')
                    && id.next() == Some('e')
                    && id.next() == Some('r')
                {
                    return Self::Super;
                }
            }
            // 'this' | 'true'
            Some('t') => match id.next() {
                Some('h') => {
                    if id.next() == Some('i') && id.next() == Some('s') {
                        return Self::This;
                    }
                }
                Some('r') => {
                    if id.next() == Some('u') && id.next() == Some('e') {
                        return Self::True;
                    }
                }
                _ => return Self::Identifier(identifier),
            },
            // 'var'
            Some('v') => {
                if id.next() == Some('a') && id.next() == Some('r') {
                    return Self::Var;
                }
            }
            // 'while'
            Some('w') => {
                if id.next() == Some('h')
                    && id.next() == Some('i')
                    && id.next() == Some('l')
                    && id.next() == Some('e')
                {
                    return Self::While;
                }
            }
            _ => return Self::Identifier(identifier),
        }
        Self::Identifier(identifier)
    }
}

#[derive(Debug, PartialEq)]
pub enum LexerError {
    InvalidToken { span: Span },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Span {
    line: usize,
    column: usize,
    char_index: usize,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// This struct is a simple ring buffer,
/// We use it to store peeked Chars.
/// It is backed by a small Boxed array, but it might grow as we add
/// chars into it.
#[derive(Debug)]
pub struct PeekBuffer<T> {
    buffer: Box<[T]>,
    // index of first slot with content
    start: usize,
    // actual content size
    count: usize,
}
impl<T: Copy + Default> PeekBuffer<T> {
    
    /// Build a new buffer
    pub fn new() -> Self {
        Self {
            buffer: vec![T::default(); BASE_PEEK_BUFFER_SIZE].into_boxed_slice(),
            start: 0_usize,
            count: 0,
        }
    }

    /// Number of item in the buffer (!= capacity)
    pub fn len(&self) -> usize {
        self.count
    }

    /// Add an item at the end of the buffer
    pub fn push_back(&mut self, item: T) {
        // if the buffer is full, copy its whole content into a new one, twice as big.
        if self.count == self.buffer.len() {
            let new_buffer_size: usize = self.buffer.len() * 2;
            let mut new_buffer = vec![T::default(); new_buffer_size].into_boxed_slice();
            for idx in 0..self.count {
                let item_idx = (self.start + idx) % self.buffer.len();
                new_buffer[idx] = self.buffer[item_idx];
            }
            self.start = 0;
            self.buffer = new_buffer;
        }
        let idx = (self.start + self.count) % self.buffer.len();
        self.buffer[idx] = item;
        self.count += 1;
    }

    /// pop the first item of the buffer
    pub fn pop_front(&mut self) -> Option<T> {
        if self.count == 0 {
            return None;
        }
        let item = self.buffer[self.start];
        self.count -= 1;
        self.start = (self.start + 1) % self.buffer.len();
        Some(item)
    }

    /// get a Ref to the index-th item in the buffer
    pub fn get<'slf>(&'slf self, index: usize) -> Option<&'slf T> {
        if index >= self.count {
            return None;
        }
        let idx = (self.start + index) % self.buffer.len();
        Some(&self.buffer[idx])
    }
}

#[derive(Debug)]
pub struct SourceCursor<'src> {
    source: Chars<'src>,
    line: usize,
    column: usize,
    absolute_index: usize,
    peeked: PeekBuffer<Option<char>>,
}

impl<'src> SourceCursor<'src> {
    fn new(source: &'src str) -> Self {
        Self {
            source: source.chars(),
            line: 1,
            column: 1,
            absolute_index: 0,
            peeked: PeekBuffer::<Option<char>>::new(),
        }
    }

    fn span(&self) -> Span {
        Span {
            line: self.line,
            column: self.column - 1,
            char_index: self.absolute_index - 1,
        }
    }

    fn new_line(&mut self) {
        self.column = 1;
        self.line += 1;
    }

    /// read a char and update position
    fn advance(&mut self) -> Option<char> {
        let maybe_char = {
            if let Some(maybe_char) = self.peeked.pop_front() {
                maybe_char
            } else {
                self.source.next()
            }
        };

        if let Some(c) = maybe_char {
            self.column += 1;
            self.absolute_index += 1;
            return Some(c);
        }
        None
    }

    fn peek(&mut self, index: usize) -> Option<char> {
        while self.peeked.len() <= index {
            self.peeked.push_back(self.source.next());
        }
        if let Some(Some(c)) = self.peeked.get(index) {
            return Some(*c);
        }
        None
    }
}

pub struct Lexer<'src> {
    cursor: SourceCursor<'src>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            cursor: SourceCursor::new(&*source),
        }
    }

    fn next_char_match(&mut self, c: char) -> bool {
        if let Some(cc) = self.cursor.peek(0) {
            return cc == c;
        }
        false
    }

    pub fn scan_next(&mut self) -> Result<Token, LexerError> {
        // read next non-whitespace character
        let c: char = {
            loop {
                if let Some(c) = self.cursor.advance() {
                    if c.is_whitespace() {
                        if c == '\n' {
                            self.cursor.new_line();
                        }
                    } else {
                        break c;
                    }
                } else {
                    return Ok(Token {
                        kind: TokenKind::Eof,
                        span: self.cursor.span(),
                    });
                }
            }
        };

        if let Some(token) = self.try_parse_alphanumeric(c) {
            return Ok(token);
        }
        if let Some(token) = self.try_parse_number(c) {
            return Ok(token);
        }
        match c {
            '(' => {
                return Ok(Token {
                    kind: TokenKind::LeftParen,
                    span: self.cursor.span(),
                })
            }
            ')' => {
                return Ok(Token {
                    kind: TokenKind::RightParen,
                    span: self.cursor.span(),
                })
            }
            '{' => {
                return Ok(Token {
                    kind: TokenKind::LeftBrace,
                    span: self.cursor.span(),
                })
            }
            '}' => {
                return Ok(Token {
                    kind: TokenKind::RightBrace,
                    span: self.cursor.span(),
                })
            }
            ';' => {
                return Ok(Token {
                    kind: TokenKind::Semicolon,
                    span: self.cursor.span(),
                })
            }
            ',' => {
                return Ok(Token {
                    kind: TokenKind::Comma,
                    span: self.cursor.span(),
                })
            }
            '.' => {
                return Ok(Token {
                    kind: TokenKind::Dot,
                    span: self.cursor.span(),
                })
            }
            '-' => {
                return Ok(Token {
                    kind: TokenKind::Minus,
                    span: self.cursor.span(),
                })
            }
            '+' => {
                return Ok(Token {
                    kind: TokenKind::Plus,
                    span: self.cursor.span(),
                })
            }
            '/' => {
                return Ok(Token {
                    kind: TokenKind::Slash,
                    span: self.cursor.span(),
                })
            }
            '*' => {
                return Ok(Token {
                    kind: TokenKind::Star,
                    span: self.cursor.span(),
                })
            }
            '!' => {
                let span = self.cursor.span();
                if self.next_char_match('=') {
                    let _ = self.cursor.advance();
                    return Ok(Token {
                        kind: TokenKind::BangEqual,
                        span,
                    });
                }
                return Ok(Token {
                    kind: TokenKind::Bang,
                    span,
                });
            }
            '=' => {
                let span = self.cursor.span();
                if self.next_char_match('=') {
                    let _ = self.cursor.advance();
                    return Ok(Token {
                        kind: TokenKind::EqualEqual,
                        span,
                    });
                }
                return Ok(Token {
                    kind: TokenKind::Equal,
                    span,
                });
            }
            '<' => {
                let span = self.cursor.span();
                if self.next_char_match('=') {
                    let _ = self.cursor.advance();
                    return Ok(Token {
                        kind: TokenKind::LessEqual,
                        span,
                    });
                }
                return Ok(Token {
                    kind: TokenKind::Less,
                    span,
                });
            }
            '>' => {
                let span = self.cursor.span();
                if self.next_char_match('=') {
                    let _ = self.cursor.advance();
                    return Ok(Token {
                        kind: TokenKind::GreaterEqual,
                        span,
                    });
                }
                return Ok(Token {
                    kind: TokenKind::Greater,
                    span,
                });
            }
            '"' => return self.parse_str_literal(),
            _ => {
                return Err(LexerError::InvalidToken {span: self.cursor.span()} );
            }
        }
    }

    fn try_parse_alphanumeric(&mut self, c: char) -> Option<Token> {
        if !c.is_ascii_alphabetic() {
            return None;
        }
        let mut identifier = String::new();
        identifier.push(c);
        let span = self.cursor.span();
        while let Some(c) = self.cursor.peek(identifier.len() - 1) {
            if !c.is_ascii_alphanumeric() {
                break;
            }
            identifier.push(c);
        }
        for _ in 0..identifier.len() {
            self.cursor.advance();
        }
        Some(Token {
            kind: TokenKind::parse_string(identifier),
            span,
        })
    }

    fn try_parse_number(&mut self, c: char) -> Option<Token> {
        if !c.is_ascii_digit() {
            return None;
        }
        let mut identifier = String::new();
        identifier.push(c);
        let span = self.cursor.span();
        while let Some(c) = self.cursor.peek(identifier.len() - 1) {
            if c.is_whitespace() {
                break;
            }
            identifier.push(c);
            if !c.is_ascii_digit() {
                if c == '.' {
                    while let Some(cc) = self.cursor.peek(identifier.len() - 1) {
                        if cc.is_whitespace() {
                            break;
                        }
                        identifier.push(cc);
                    }
                    break;
                } else {
                    return None;
                }
            }
        }
        for _ in 0..identifier.len() {
            self.cursor.advance();
        }
        Some(Token {
            kind: TokenKind::Number(identifier.parse().unwrap()),
            span,
        })
    }

    fn parse_str_literal(&mut self) -> Result<Token, LexerError> {
        let span = self.cursor.span();
        let mut str_literal = String::new();
        while let Some(c) = self.cursor.advance() {
            if c == '"' {
                return Ok(Token {
                    kind: TokenKind::Str(str_literal),
                    span,
                });
            }
            str_literal.push(c);
        }
        Err(LexerError::InvalidToken { span })
    }
}

#[test]
fn test_cursor() {
    let mut cursor = SourceCursor::new("01");
    assert_eq!(cursor.peek(0), Some('0'));
    assert_eq!(cursor.peek(1), Some('1'));
    assert_eq!(cursor.peek(2), None);
    assert_eq!(cursor.advance(), Some('0'));
    assert_eq!(cursor.advance(), Some('1'));
    assert_eq!(cursor.peek(0), None);
    assert_eq!(cursor.advance(), None);
}

#[test]
fn should_parse_static_tokens() {
    // '('
    assert_eq!(
        Lexer::new("(").scan_next(),
        Ok(Token {
            kind: TokenKind::LeftParen,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // ')'
    assert_eq!(
        Lexer::new(")").scan_next(),
        Ok(Token {
            kind: TokenKind::RightParen,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '{'
    assert_eq!(
        Lexer::new("{").scan_next(),
        Ok(Token {
            kind: TokenKind::LeftBrace,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '}'
    assert_eq!(
        Lexer::new("}").scan_next(),
        Ok(Token {
            kind: TokenKind::RightBrace,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // ';'
    assert_eq!(
        Lexer::new(";").scan_next(),
        Ok(Token {
            kind: TokenKind::Semicolon,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // ','
    assert_eq!(
        Lexer::new(",").scan_next(),
        Ok(Token {
            kind: TokenKind::Comma,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '.'
    assert_eq!(
        Lexer::new(".").scan_next(),
        Ok(Token {
            kind: TokenKind::Dot,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '-'
    assert_eq!(
        Lexer::new("-").scan_next(),
        Ok(Token {
            kind: TokenKind::Minus,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '+'
    assert_eq!(
        Lexer::new("+").scan_next(),
        Ok(Token {
            kind: TokenKind::Plus,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '/'
    assert_eq!(
        Lexer::new("/").scan_next(),
        Ok(Token {
            kind: TokenKind::Slash,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '*'
    assert_eq!(
        Lexer::new("*").scan_next(),
        Ok(Token {
            kind: TokenKind::Star,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '!'
    assert_eq!(
        Lexer::new("!").scan_next(),
        Ok(Token {
            kind: TokenKind::Bang,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '!='
    assert_eq!(
        Lexer::new("!=").scan_next(),
        Ok(Token {
            kind: TokenKind::BangEqual,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '='
    assert_eq!(
        Lexer::new("=").scan_next(),
        Ok(Token {
            kind: TokenKind::Equal,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '=='
    assert_eq!(
        Lexer::new("==").scan_next(),
        Ok(Token {
            kind: TokenKind::EqualEqual,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '<'
    assert_eq!(
        Lexer::new("<").scan_next(),
        Ok(Token {
            kind: TokenKind::Less,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '<='
    assert_eq!(
        Lexer::new("<=").scan_next(),
        Ok(Token {
            kind: TokenKind::LessEqual,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '>'
    assert_eq!(
        Lexer::new(">").scan_next(),
        Ok(Token {
            kind: TokenKind::Greater,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // '>='
    assert_eq!(
        Lexer::new(">=").scan_next(),
        Ok(Token {
            kind: TokenKind::GreaterEqual,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
}

#[test]
fn should_parse_number_tokens() {
    // single digit
    assert_eq!(
        Lexer::new("0").scan_next(),
        Ok(Token {
            kind: TokenKind::Number(0.),
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // several digits
    assert_eq!(
        Lexer::new("10").scan_next(),
        Ok(Token {
            kind: TokenKind::Number(10.),
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // floating point
    assert_eq!(
        Lexer::new("10.1").scan_next(),
        Ok(Token {
            kind: TokenKind::Number(10.1),
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
}

#[test]
fn should_parse_identifier_tokens() {
    assert_eq!(
        Lexer::new("a0").scan_next(),
        Ok(Token {
            kind: TokenKind::Identifier("a0".to_string()),
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    // same with space after
    assert_eq!(
        Lexer::new("a0 ").scan_next(),
        Ok(Token {
            kind: TokenKind::Identifier("a0".to_string()),
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
}

#[test]
fn should_parse_keyword_tokens() {
    assert_eq!(
        Lexer::new("and").scan_next(),
        Ok(Token {
            kind: TokenKind::And,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("class").scan_next(),
        Ok(Token {
            kind: TokenKind::Class,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("else").scan_next(),
        Ok(Token {
            kind: TokenKind::Else,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("false").scan_next(),
        Ok(Token {
            kind: TokenKind::False,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("for").scan_next(),
        Ok(Token {
            kind: TokenKind::For,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("fun").scan_next(),
        Ok(Token {
            kind: TokenKind::Fun,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("if").scan_next(),
        Ok(Token {
            kind: TokenKind::If,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("nil").scan_next(),
        Ok(Token {
            kind: TokenKind::Nil,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("or").scan_next(),
        Ok(Token {
            kind: TokenKind::Or,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("print").scan_next(),
        Ok(Token {
            kind: TokenKind::Print,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("return").scan_next(),
        Ok(Token {
            kind: TokenKind::Return,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("super").scan_next(),
        Ok(Token {
            kind: TokenKind::Super,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("this").scan_next(),
        Ok(Token {
            kind: TokenKind::This,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("true").scan_next(),
        Ok(Token {
            kind: TokenKind::True,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("var").scan_next(),
        Ok(Token {
            kind: TokenKind::Var,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
    assert_eq!(
        Lexer::new("while").scan_next(),
        Ok(Token {
            kind: TokenKind::While,
            span: Span {
                line: 1,
                column: 1,
                char_index: 0
            }
        })
    );
}

#[test]
fn should_grow_without_issue() {
    let mut buffer: PeekBuffer<usize> = PeekBuffer::new();
    // overflow the initial capacity of the ring buffer
    // assert that the realloaction did not fail
    for i in 0..(BASE_PEEK_BUFFER_SIZE * 3) {
        buffer.push_back(i);
    }

    // check that the values are still there
    for i in 0..buffer.len() {
        assert_eq!(*(buffer.get(i).unwrap()), i);
    }

    // pop all content, in order
    for i in 0..(BASE_PEEK_BUFFER_SIZE * 3) {
        let item = buffer.pop_front();
        assert_eq!(item, Some(i));
    }

    assert_eq!(buffer.len(), 0);
}
