use super::ast::NodeId;
use lexer::{LexerError, Span, Token, TokenKind, Tokenize};

#[derive(Debug)]
pub enum ParseError {
    ExpectedToken(&'static str),
    ScanningError(LexerError),
    ExpectedExpression(&'static str),
    Starved,
}

/// Handles the parser state while navigating the stream
/// of tokens.
pub struct ParserState<'input> {
    lexer: Box<dyn Tokenize + 'input>,
    current: Option<Token>,
    previous: Option<Token>,
    /// used to label AST node, as we create them
    ast_id_inc: NodeId,
}

impl<'input> ParserState<'input> {
    pub fn new(lexer: Box<dyn Tokenize + 'input>) -> Self {
        Self {
            lexer,
            current: None,
            previous: None,
            ast_id_inc: 0,
        }
    }

    /// Consume one Token from the lexer,
    /// and update `self.previous` and `self.current`
    pub fn advance(&mut self) -> Result<(), ParseError> {
        self.previous = self.current.take();
        self.current =
            Some(self.lexer.scan_next().map_err(ParseError::ScanningError)?);
        Ok(())
    }

    /// get ownership of the current token
    pub fn take_current(&mut self) -> Result<Token, ParseError> {
        self.current.take().ok_or(ParseError::Starved)
    }
    /// get ownership of the previous token
    pub fn take_previous(&mut self) -> Result<Token, ParseError> {
        self.previous.take().ok_or(ParseError::Starved)
    }

    /// The current token
    pub fn current<'token>(&'input self) -> Result<&'token Token, ParseError>
    where
        'input: 'token,
    {
        self.current.as_ref().ok_or(ParseError::Starved)
    }

    /// The last token we parsed
    pub fn previous<'token>(&'input self) -> Result<&'token Token, ParseError>
    where
        'input: 'token,
    {
        self.previous.as_ref().ok_or(ParseError::Starved)
    }

    /// consume one token from the lexer,
    /// return an error if it doesn't match `kind`
    pub fn consume(
        &mut self,
        _kind: TokenKind,
        err_msg: &'static str,
    ) -> Result<(), ParseError> {
        self.advance()?;
        if !matches!(&self.previous()?.kind, _kind) {
            return Err(ParseError::ExpectedToken(err_msg));
        }
        Ok(())
    }

    /// returns `true` if the current token if of type `kind`
    /// only check the variant type, not embeded values( if any)
    pub fn check(&self, kind: TokenKind) -> Result<bool, ParseError> {
        Ok(std::mem::discriminant(&kind)
            == std::mem::discriminant(&self.current()?.kind))
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

    /// get the current position of the cursor,
    /// (in the source input string).
    pub fn current_position(&mut self) -> Span {
        self.lexer.current_position()
    }

    /// return a fresh, unique, NodeId
    pub fn new_node_id(&mut self) -> NodeId {
        let id = self.ast_id_inc;
        self.ast_id_inc += 1;
        id
    }
}
