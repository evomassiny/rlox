use super::file_reader::ReaderPeeker;
use super::source::{PeekOffset, SourceInput, Span};
use super::str_reader::StrPeeker;
use super::token::TokenKind;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, PartialEq)]
pub enum LexerError {
    InvalidToken { span: Span },
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

/// This struct handle the lexing 
/// (sometimes called "scanning") of a string input.
///
/// It emits `Token` as it's reading  its input.
///
/// It is generic over its input, as long as it
/// implements the `PeekOffset` trait.
/// This way, it can be used to lex an utf-8 string,
/// or a file.
pub struct Lexer<T> {
    input: SourceInput<T>,
}

impl Lexer<ReaderPeeker<File>> {
    /// build a Lexer from a file,
    /// the lexer will lazyly parse as utf-8 the file content when needed.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self {
            input: SourceInput::from_path(path)?,
        })
    }
}

impl<'src, const SIZE: usize> Lexer<StrPeeker<'src, { SIZE }>> {
    /// Build a lexer from an utf-8 string.
    pub fn from_str(src: &'src str) -> Self {
        Self {
            input: SourceInput::from_str(src),
        }
    }
}

impl<T: PeekOffset> Lexer<T> {
    fn next_char_match(&mut self, c: char) -> bool {
        if let Some(cc) = self.input.peek(0) {
            return cc == c;
        }
        false
    }

    pub fn scan_next(&mut self) -> Result<Token, LexerError> {
        // read next non-whitespace character
        let c: char = {
            loop {
                if let Some(c) = self.input.advance() {
                    if c.is_whitespace() {
                        if c == '\n' {
                            self.input.new_line();
                        }
                    } else {
                        break c;
                    }
                } else {
                    return Ok(Token {
                        kind: TokenKind::Eof,
                        span: self.input.span(),
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
                    span: self.input.span(),
                })
            }
            ')' => {
                return Ok(Token {
                    kind: TokenKind::RightParen,
                    span: self.input.span(),
                })
            }
            '{' => {
                return Ok(Token {
                    kind: TokenKind::LeftBrace,
                    span: self.input.span(),
                })
            }
            '}' => {
                return Ok(Token {
                    kind: TokenKind::RightBrace,
                    span: self.input.span(),
                })
            }
            ';' => {
                return Ok(Token {
                    kind: TokenKind::Semicolon,
                    span: self.input.span(),
                })
            }
            ',' => {
                return Ok(Token {
                    kind: TokenKind::Comma,
                    span: self.input.span(),
                })
            }
            '.' => {
                return Ok(Token {
                    kind: TokenKind::Dot,
                    span: self.input.span(),
                })
            }
            '-' => {
                return Ok(Token {
                    kind: TokenKind::Minus,
                    span: self.input.span(),
                })
            }
            '+' => {
                return Ok(Token {
                    kind: TokenKind::Plus,
                    span: self.input.span(),
                })
            }
            '/' => {
                return Ok(Token {
                    kind: TokenKind::Slash,
                    span: self.input.span(),
                })
            }
            '*' => {
                return Ok(Token {
                    kind: TokenKind::Star,
                    span: self.input.span(),
                })
            }
            '!' => {
                let span = self.input.span();
                if self.next_char_match('=') {
                    let _ = self.input.advance();
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
                let span = self.input.span();
                if self.next_char_match('=') {
                    let _ = self.input.advance();
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
                let span = self.input.span();
                if self.next_char_match('=') {
                    let _ = self.input.advance();
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
                let span = self.input.span();
                if self.next_char_match('=') {
                    let _ = self.input.advance();
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
                return Err(LexerError::InvalidToken {
                    span: self.input.span(),
                });
            }
        }
    }

    fn try_parse_alphanumeric(&mut self, c: char) -> Option<Token> {
        if !c.is_ascii_alphabetic() {
            return None;
        }
        let mut identifier = String::new();
        identifier.push(c);
        let span = self.input.span();
        while let Some(c) = self.input.peek(identifier.len() - 1) {
            if !c.is_ascii_alphanumeric() {
                break;
            }
            identifier.push(c);
        }
        for _ in 0..identifier.len() {
            self.input.advance();
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
        let span = self.input.span();
        while let Some(c) = self.input.peek(identifier.len() - 1) {
            if c.is_whitespace() {
                break;
            }
            identifier.push(c);
            if !c.is_ascii_digit() {
                if c == '.' {
                    while let Some(cc) = self.input.peek(identifier.len() - 1) {
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
            self.input.advance();
        }
        Some(Token {
            kind: TokenKind::Number(identifier.parse().unwrap()),
            span,
        })
    }

    fn parse_str_literal(&mut self) -> Result<Token, LexerError> {
        let span = self.input.span();
        let mut str_literal = String::new();
        while let Some(c) = self.input.advance() {
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
fn should_parse_static_tokens() {
    type StrLexer<'a> = Lexer<StrPeeker<'a, 64>>;
    // '('
    assert_eq!(
        StrLexer::from_str("(").scan_next(),
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
        StrLexer::from_str(")").scan_next(),
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
        StrLexer::from_str("{").scan_next(),
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
        StrLexer::from_str("}").scan_next(),
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
        StrLexer::from_str(";").scan_next(),
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
        StrLexer::from_str(",").scan_next(),
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
        StrLexer::from_str(".").scan_next(),
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
        StrLexer::from_str("-").scan_next(),
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
        StrLexer::from_str("+").scan_next(),
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
        StrLexer::from_str("/").scan_next(),
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
        StrLexer::from_str("*").scan_next(),
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
        StrLexer::from_str("!").scan_next(),
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
        StrLexer::from_str("!=").scan_next(),
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
        StrLexer::from_str("=").scan_next(),
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
        StrLexer::from_str("==").scan_next(),
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
        StrLexer::from_str("<").scan_next(),
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
        StrLexer::from_str("<=").scan_next(),
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
        StrLexer::from_str(">").scan_next(),
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
        StrLexer::from_str(">=").scan_next(),
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
    type StrLexer<'a> = Lexer<StrPeeker<'a, 64>>;
    // single digit
    assert_eq!(
        StrLexer::from_str("0").scan_next(),
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
        StrLexer::from_str("10").scan_next(),
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
        StrLexer::from_str("10.1").scan_next(),
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
    type StrLexer<'a> = Lexer<StrPeeker<'a, 64>>;
    assert_eq!(
        StrLexer::from_str("a0").scan_next(),
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
        StrLexer::from_str("a0 ").scan_next(),
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
    type StrLexer<'a> = Lexer<StrPeeker<'a, 64>>;
    assert_eq!(
        StrLexer::from_str("and").scan_next(),
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
        StrLexer::from_str("class").scan_next(),
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
        StrLexer::from_str("else").scan_next(),
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
        StrLexer::from_str("false").scan_next(),
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
        StrLexer::from_str("for").scan_next(),
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
        StrLexer::from_str("fun").scan_next(),
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
        StrLexer::from_str("if").scan_next(),
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
        StrLexer::from_str("nil").scan_next(),
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
        StrLexer::from_str("or").scan_next(),
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
        StrLexer::from_str("print").scan_next(),
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
        StrLexer::from_str("return").scan_next(),
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
        StrLexer::from_str("super").scan_next(),
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
        StrLexer::from_str("this").scan_next(),
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
        StrLexer::from_str("true").scan_next(),
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
        StrLexer::from_str("var").scan_next(),
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
        StrLexer::from_str("while").scan_next(),
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
