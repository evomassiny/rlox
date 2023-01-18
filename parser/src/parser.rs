use super::ast::{Expr, ExprKind, LiteralKind, Stmt, StmtKind};
use super::cursor::{Cursor, ParseError};
use lexer::{Span, Token, TokenKind, Tokenize};

/// precedence order
/// NOTE: higher precedence means less expressions.
/// eg: in `A*B+C`, * concerns 2 expressions, + concerns 4
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug)]
pub enum Precedence {
    /// means stop parsing
    None = 0,
    /// =
    Assignement = 1,
    /// or
    Or = 2,
    /// and
    And = 3,
    /// ==, !=
    Equality = 4,
    /// <, >, <=, >=
    Comparison = 5,
    /// +, -
    Term = 6,
    /// *, /
    Factor = 7,
    /// !, -
    Unary = 8,
    /// . ()
    Call = 9,
    Primary = 10,
}

/// Expression parser
/// Implements a Pratt parser,
/// using 2 tables of parsing rules
/// * one for infix expression
/// * one for prefix expression
pub struct ExprParser<'a>(&'a mut Cursor);

/// a function that parses an expression from a cursor, given a prefix TokenKind
type PrefixParserFn = fn(&mut Cursor, bool) -> Result<Expr, ParseError>;

/// a function that parses an expression from a cursor and the previously parsed
/// expression, given an infix TokenKind
type InfixParserFn = fn(&mut Cursor, Expr, bool) -> Result<Expr, ParseError>;

impl<'a> ExprParser<'a> {
    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        Self::parse_precendence(self.0, Precedence::Assignement)
    }

    fn parse_precendence(cursor: &mut Cursor, precedence: Precedence) -> Result<Expr, ParseError> {
        let _ = cursor.advance()?;
        let prefix_fn = Self::get_prefix_handler(&cursor.previous()?.kind)
            .ok_or(ParseError::ExpectedExpression)?;
        let can_assign = precedence <= Precedence::Assignement;

        let mut expr = prefix_fn(cursor, can_assign)?;
        dbg!(&expr);

        while Self::get_token_precedence(&cursor.current()?.kind) >= precedence {
            let _ = cursor.advance();
            // call the rule associated with handling
            // expressing **CONTAINING** this token
            let infix_fn = Self::get_infix_handler(&cursor.previous()?.kind)
                .ok_or(ParseError::ExpectedExpression)?;
            expr = infix_fn(cursor, expr, can_assign)?;
        }
        if can_assign && cursor.matches(TokenKind::Equal)? {
            return Err(ParseError::ExpectedToken(
                "Invalid assignement target.".into(),
            ));
        }
        Ok(expr)
    }

    fn parse_grouping(cursor: &mut Cursor, can_assign: bool) -> Result<Expr, ParseError> {
        todo!();
    }

    /// Build a Literal expression from a Number Token.
    fn parse_number(cursor: &mut Cursor, _can_assign: bool) -> Result<Expr, ParseError> {
        let Token { kind: TokenKind::Number(value), span } = cursor.take_previous()? else {
           return Err(ParseError::ExpectedToken("Expected a number".to_string()));
        };
        Ok(Expr {
            kind: ExprKind::Literal(LiteralKind::Num(value)),
            span,
        })
    }

    /// Build a sum,
    /// (assumes a '+' or '-' token has just been parse)
    fn parse_sum(cursor: &mut Cursor, lhs: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        // rhs => right hand side
        // lhs => left hand side
        let Token { kind, span } = cursor.take_previous()?;
        let rhs = Self::parse_precendence(cursor, Precedence::Factor)?;
        Ok(Expr {
            kind: ExprKind::Binary(Box::new(lhs), kind, Box::new(rhs)),
            span,
        })
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression
    fn get_prefix_handler(kind: &TokenKind) -> Option<PrefixParserFn> {
        match kind {
            &TokenKind::LeftParen => todo!(),
            &TokenKind::Minus => todo!(),
            &TokenKind::Bang => todo!(),
            &TokenKind::Identifier(_) => todo!(),
            &TokenKind::Str(_) => todo!(),
            &TokenKind::Number(_) => Some(Self::parse_number),
            &TokenKind::False => todo!(),
            &TokenKind::Nil => todo!(),
            &TokenKind::Super => todo!(),
            &TokenKind::This => todo!(),
            &TokenKind::True => todo!(),
            _ => None,
        }
    }

    fn get_token_precedence(kind: &TokenKind) -> Precedence {
        match kind {
            &TokenKind::Plus | &TokenKind::Minus => Precedence::Term,
            _ => Precedence::None,
        }
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression if we found the token while parsing a bigger expression
    fn get_infix_handler(kind: &TokenKind) -> Option<InfixParserFn> {
        match kind {
            &TokenKind::LeftParen => todo!(),
            &TokenKind::Dot => todo!(),
            &TokenKind::Minus => todo!(),
            &TokenKind::Plus => Some(Self::parse_sum),
            &TokenKind::Slash => todo!(),
            &TokenKind::BangEqual => todo!(),
            &TokenKind::EqualEqual => todo!(),
            &TokenKind::Greater => todo!(),
            &TokenKind::GreaterEqual => todo!(),
            &TokenKind::Less => todo!(),
            &TokenKind::LessEqual => todo!(),
            &TokenKind::And => todo!(),
            &TokenKind::Or => todo!(),
            _ => None,
        }
    }
}

/// Parse statements using an explicit recursive
/// descent parser, and expressions using a Pratt Parser.
pub struct Parser {
    cursor: Cursor,
    stmt_stack: Vec<Stmt>,
}

impl Parser {
    pub fn new(lexer: Box<dyn Tokenize>) -> Self {
        let cursor = Cursor::new(lexer);
        Self {
            cursor,
            stmt_stack: Vec::new(),
        }
    }

    /**
     * declaration -> varDeclaration
     *                | classDeclaration
     *                | funDeclaration
     *                | statement;
     */
    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            TokenKind::Class => {
                self.cursor.advance();
                self.class_declaration()
            }
            TokenKind::Var => {
                self.cursor.advance();
                self.var_declaration()
            }
            TokenKind::Fun => {
                self.cursor.advance();
                self.fun_declaration()
            }
            _ => self.statement(),
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn fun_declaration(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }

    fn block_statement(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        todo!()
    }
    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
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
    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            TokenKind::Print => {
                self.cursor.advance();
                self.print_statement()
            }
            TokenKind::LeftBrace => {
                self.cursor.advance();
                self.block_statement()
            }
            TokenKind::If => {
                self.cursor.advance();
                self.if_statement()
            }
            TokenKind::Return => {
                self.cursor.advance();
                self.return_statement()
            }
            TokenKind::While => {
                self.cursor.advance();
                self.while_statement()
            }
            TokenKind::For => {
                self.cursor.advance();
                self.for_statement()
            }
            _ => self.expression_statement(),
        }
    }

    /// parse an expression followed by a semicolon.
    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let mut expression_parser = ExprParser(&mut self.cursor);
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
    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.cursor.advance()?;

        let mut statements = Vec::new();
        while !self.cursor.matches(TokenKind::Eof)? {
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
