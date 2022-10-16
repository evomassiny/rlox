use crate::lexer::{LexerError, Token, TokenKind, Tokenize};

#[derive(Debug)]
pub enum ParseError {
    ExpectedToken(String),
    ScanningError(LexerError),
    ExpectedExpression,
    Starved,
    CompilationError,
}

pub struct Cursor {
    lexer: Box<dyn Tokenize>,
    current: Option<Token>,
    previous: Option<Token>,
}

impl Cursor {
    pub fn new(lexer: Box<dyn Tokenize>) -> Self {
        Self {
            lexer,
            current: None,
            previous: None,
        }
    }

    /// Consume one Token from the lexer,
    /// and update `self.previous` and `self.current`
    pub fn advance(&mut self) -> Result<(), ParseError> {
        self.previous = self.current.take();
        self.current = Some(self.lexer.scan_next().map_err(ParseError::ScanningError)?);
        Ok(())
    }

    /// The current token
    pub fn current<'a>(&'a self) -> Result<&'a Token, ParseError> {
        self.current.as_ref().ok_or(ParseError::Starved)
    }

    /// The last token we parsed
    pub fn previous<'a>(&'a self) -> Result<&'a Token, ParseError> {
        self.previous.as_ref().ok_or(ParseError::Starved)
    }

    /// consume one token from the lexer,
    /// return an error if it doesn't match `kind`
    pub fn consume(&mut self, _kind: TokenKind, err_msg: &str) -> Result<(), ParseError> {
        let token = self.lexer.scan_next().map_err(ParseError::ScanningError)?;
        if !matches!(token.kind, _kind) {
            return Err(ParseError::ExpectedToken(err_msg.into()));
        }
        Ok(())
    }

    /// returns `true` if the current token if of type `kind`
    pub fn check(&self, kind: TokenKind) -> Result<bool, ParseError> {
        Ok(kind == self.current()?.kind)
    }

    /// returns `true` if the current token if of type `kind`,
    /// if so, also advance of one token.
    pub fn matches(&mut self, kind: TokenKind) -> Result<bool, ParseError> {
        if !self.check(kind)? {
            return Ok(false);
        }
        self.advance()?;
        Ok(true)
    }

    /// move the cursor to the next
    /// statetment (for error recovery).
    pub fn move_to_next_stmt(&mut self) {
        while !matches!(
            self.current,
            Some(Token {
                kind: TokenKind::Eof,
                ..
            })
        ) {
            if let Some(Token {
                kind: TokenKind::Semicolon,
                ..
            }) = self.previous
            {
                return;
            }
            match self.current {
                Some(Token {
                    kind: TokenKind::Class,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::Fun,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::Var,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::For,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::If,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::While,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::Print,
                    ..
                }) => return,
                Some(Token {
                    kind: TokenKind::Return,
                    ..
                }) => return,
                _ => {
                    let _ = self.advance();
                }
            }
        }
    }
}
