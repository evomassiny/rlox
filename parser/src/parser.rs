use super::ast::{Expr, ExprKind, Stmt, StmtKind};
use super::cursor::{Cursor, ParseError};
use lexer::{Span, TokenKind, Tokenize};

/// precedence order
/// NOTE: higher precedence means less expressions.
/// eg: in `A*B+C`, * concerns 2 expressions, + concerns 4
#[repr(u8)]
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Precedence {
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
pub struct ExprParser<'a>(&'a mut Cursor);

/// a function that parses an expression from a cursor, given a prefix TokenKind
type PrefixParserFn = fn(&mut Cursor, bool) -> Result<Expr, ParseError>;

/// a function that parses an expression from a cursor and the previously parsed
/// expression, given an infix TokenKind
type InfixParserFn = fn(&mut Cursor, Expr, bool) -> Result<Expr, ParseError>;

impl<'a> ExprParser<'a> {
    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.parse_precendence(Precedence::Assignement)
    }

    fn parse_precendence(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        let (prefix_fn, prefix_expr_precedence) = Self::get_prefix_handler(&self.0.current()?.kind)
            .ok_or(ParseError::ExpectedExpression)?;
        let can_assign = precedence <= Precedence::Assignement;

        let mut expr = prefix_fn(&mut self.0, can_assign)?;

        while precedence <= prefix_expr_precedence {
            self.0.advance()?;
            // call the rule associated with handling
            // expressing **CONTAINING** this token
            let infix_fn = Self::get_infix_handler(&self.0.previous()?.kind)
                .ok_or(ParseError::ExpectedExpression)?;
            expr = infix_fn(self.0, expr, can_assign)?;
        }
        if can_assign && self.0.matches(TokenKind::Equal)? {
            return Err(ParseError::ExpectedToken(
                "Invalid assignement target.".into(),
            ));
        }
        Ok(expr)
    }

    fn grouping(cursor: &mut Cursor, can_assign: bool) -> Result<Expr, ParseError> {
        todo!();
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression
    fn get_prefix_handler(kind: &TokenKind) -> Option<(PrefixParserFn, Precedence)> {
        match kind {
            &TokenKind::LeftParen => Some((Self::grouping, Precedence::Call)),
            &TokenKind::Minus => todo!(),
            &TokenKind::Bang => todo!(),
            &TokenKind::Identifier(_) => todo!(),
            &TokenKind::Str(_) => todo!(),
            &TokenKind::Number(_) => todo!(),
            &TokenKind::False => todo!(),
            &TokenKind::Nil => todo!(),
            &TokenKind::Super => todo!(),
            &TokenKind::This => todo!(),
            &TokenKind::True => todo!(),
            _ => None,
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
            &TokenKind::Plus => todo!(),
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

/// Implements a Pratt parser,
/// using a table of parsing rules
/// (see `ParseRule::get_parsing_rule_for_token()`)
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
    fn declaration(&mut self) -> Result<(), ParseError> {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            TokenKind::Class => {
                self.cursor.advance();
                self.class_declaration()?;
            }
            TokenKind::Var => {
                self.cursor.advance();
                self.var_declaration()?;
            }
            TokenKind::Fun => {
                self.cursor.advance();
                self.fun_declaration()?;
            }
            _ => {
                self.statement();
            }
        }
        Ok(())
    }

    fn class_declaration(&mut self) -> Result<(), ParseError> {
        todo!()
    }

    fn fun_declaration(&mut self) -> Result<(), ParseError> {
        todo!()
    }

    fn var_declaration(&mut self) -> Result<(), ParseError> {
        todo!()
    }

    fn block_statement(&mut self) -> Result<(), ParseError> {
        todo!()
    }
    fn if_statement(&mut self) -> Result<(), ParseError> {
        todo!()
    }
    fn return_statement(&mut self) -> Result<(), ParseError> {
        todo!()
    }
    fn while_statement(&mut self) -> Result<(), ParseError> {
        todo!()
    }
    fn for_statement(&mut self) -> Result<(), ParseError> {
        todo!()
    }
    fn expression_statement(&mut self) -> Result<(), ParseError> {
        todo!()
    }
    fn print_statement(&mut self) -> Result<(), ParseError> {
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
    fn statement(&mut self) -> Result<(), ParseError> {
        let current: &TokenKind = &self.cursor.current()?.kind;
        match *current {
            TokenKind::Print => {
                self.cursor.advance();
                self.print_statement()?;
            }
            TokenKind::LeftBrace => {
                self.cursor.advance();
                self.block_statement()?;
            }
            TokenKind::If => {
                self.cursor.advance();
                self.if_statement()?;
            }
            TokenKind::Return => {
                self.cursor.advance();
                self.return_statement()?;
            }
            TokenKind::While => {
                self.cursor.advance();
                self.while_statement()?;
            }
            TokenKind::For => {
                self.cursor.advance();
                self.for_statement()?;
            }
            _ => {
                self.expression_statement();
            }
        }
        Ok(())
    }

    /// Parse an expression
    fn expression(&mut self, _can_assign: bool) -> Result<(), ParseError> {
        let mut expression_parser = ExprParser(&mut self.cursor);
        let expr = expression_parser.parse()?;
        Ok(())
    }

    /// Parse source into statements
    pub fn parse(&mut self) -> Result<(), ParseError> {
        self.cursor.advance()?;
        // self.stmt_stack.push([> TODO <]);

        while !self.cursor.matches(TokenKind::Eof)? {
            // parse declaration, forward
            // errors (if any) and move the token cursor to the next
            // statement. (allow recovery in REPL environment)
            if let Err(e) = self.declaration() {
                self.cursor.move_to_next_stmt();
                return Err(e);
            }
        }

        Ok(())
    }
}
