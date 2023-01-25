use super::ast::{Expr, ExprKind, LiteralKind, Stmt, StmtKind};
use super::cursor::{Cursor, ParseError};
use super::expr_parser::ExprParser;
use lexer::{Span, Token, TokenKind, Tokenize};

/// Parse statements using an explicit recursive
/// descent parser, and expressions using a Pratt Parser.
pub struct StmtParser<'input> {
    cursor: Cursor<'input>,
}

impl<'input> StmtParser<'input> {
    pub fn new(lexer: Box<dyn Tokenize + 'input>) -> Self {
        let cursor = Cursor::new(lexer);
        Self { cursor }
    }

    /**
     * declaration -> varDeclaration
     *                | classDeclaration
     *                | funDeclaration
     *                | statement;
     */
    fn declaration<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            /*
            TokenKind::Class => {
                self.cursor.advance()?;
                self.class_declaration()
            }
            TokenKind::Var => {
                self.cursor.advance()?;
                self.var_declaration()
            }
            TokenKind::Fun => {
                self.cursor.advance()?;
                self.fun_declaration()
            }
            */
            _ => self.statement(),
        }
    }

    fn class_declaration(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn fun_declaration(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn var_declaration(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn block_statement(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn if_statement(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn return_statement(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn while_statement(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn for_statement(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn print_statement(&'input mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    /**
     * statement -> printStatement
     *              | blockStatement
     *              | ifStatement
     *              | returnStatement
     *              | whileStatement
     *              | forStatement
     *              | expressionStatement
     *              ;
     */
    fn statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            /*
            TokenKind::Print => {
                self.cursor.advance()?;
                self.print_statement()
            }
            TokenKind::LeftBrace => {
                self.cursor.advance()?;
                self.block_statement()
            }
            TokenKind::If => {
                self.cursor.advance()?;
                self.if_statement()
            }
            TokenKind::Return => {
                self.cursor.advance()?;
                self.return_statement()
            }
            TokenKind::While => {
                self.cursor.advance()?;
                self.while_statement()
            }
            TokenKind::For => {
                self.cursor.advance()?;
                self.for_statement()
            }
            */
            _ => self.expression_statement(),
        }
    }

    /// parse an expression followed by a semicolon.
    fn expression_statement<'parser>(&'parser mut self) -> Result<Stmt, ParseError>
    where
        'input: 'parser,
    {
        let mut expression_parser = ExprParser::new(&mut self.cursor);
        let expr = expression_parser.parse()?;
        if let Ok(Token {
            kind: TokenKind::Semicolon,
            ..
        }) = self.cursor.current()
        {
            self.cursor.advance()?;
            Ok(Stmt {
                span: expr.span.clone(),
                kind: StmtKind::Expr(Box::new(expr)),
            })
        } else {
            Err(ParseError::ExpectedToken("missing ';'".to_string()))
        }
    }

    /// Parse source into statements
    pub fn parse<'parser>(&'parser mut self) -> Result<Vec<Stmt>, ParseError>
    where
        'input: 'parser,
    {
        let _ = self.cursor.advance()?;

        let mut statements = Vec::new();
        loop {
            if self.cursor.matches(TokenKind::Eof)? {
                break;
            }
            // parse declaration, forward
            // errors (if any) and move the token cursor to the next
            // statement. (allow recovery in REPL environment)
            dbg!(self.cursor.current());
            match self.declaration() {
                Err(e) => {
                    self.cursor.move_to_next_stmt();
                    return Err(e);
                }
                Ok(stmt) => statements.push(stmt),
            }
        }

        Ok(statements)
    }
}
