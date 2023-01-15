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

/// What to do when encountering a given TokenKind
pub struct ParseRule {
    /// if we are at the start of an expression / stmt
    prefix: Option<fn(&mut Parser, bool) -> Result<(), ParseError>>,
    /// if we are in the middle of parsion an expression
    infix: Option<fn(&mut Parser, bool) -> Result<(), ParseError>>,
    /// the "binding" power of the TokenKind
    precedence: Precedence,
}

/// Implements a Pratt parser,
/// using a table of parsing rules
/// (see `Self::get_parsing_rule_for_token()`)
pub struct Parser {
    cursor: Cursor,
    stmt_stack: Vec<Stmt>,
    expr_stack: Vec<Expr>,
}

impl Parser {
    pub fn new(lexer: Box<dyn Tokenize>) -> Self {
        let cursor = Cursor::new(lexer);
        Self {
            cursor,
            stmt_stack: Vec::new(),
            expr_stack: Vec::new(),
        }
    }

    /**
     * declaration -> varDeclaration
     *                | classDeclaration
     *                | funDeclaration
     *                | statement;
     */
    fn declaration(&mut self) -> Result<(), ParseError> {
        if self.cursor.matches(TokenKind::Class)? {
            self.class_declaration()?;
        } else if self.cursor.matches(TokenKind::Var)? {
            self.var_declaration()?;
        } else if self.cursor.matches(TokenKind::Fun)? {
            self.fun_declaration()?;
        } else {
            self.statement()?;
        }
        Ok(())
    }

    fn class_declaration(&mut self) -> Result<(), ParseError> {
        Ok(())
    }

    fn fun_declaration(&mut self) -> Result<(), ParseError> {
        Ok(())
    }

    fn var_declaration(&mut self) -> Result<(), ParseError> {
        Ok(())
    }

    fn statement(&mut self) -> Result<(), ParseError> {
        Ok(())
    }

    /// Parse an expression
    fn expression(&mut self, _can_assign: bool) -> Result<(), ParseError> {
        Ok(())
    }

    fn grouping(&mut self, can_assign: bool) -> Result<(), ParseError> {
        self.expression(can_assign)?;
        self.cursor
            .consume(TokenKind::RightParen, "Expecting closing parenthesis.")
    }

    /// Return, depending of the variant of `kind`
    /// which parsing routine we should use to parse the
    /// expression, depending if we found the token
    /// in the middle an expression parsion, of a the start.
    fn get_parsing_rule_for_token(kind: &TokenKind) -> &'static ParseRule {
        match kind {
            &TokenKind::LeftParen => &ParseRule {
                prefix: Some(Self::grouping),
                infix: None,
                precedence: Precedence::Call,
            },
            &TokenKind::RightParen => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::LeftBrace => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::RightBrace => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Comma => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Dot => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Minus => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Plus => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Semicolon => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Slash => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Star => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Bang => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::BangEqual => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Equal => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::EqualEqual => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Greater => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::GreaterEqual => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Less => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::LessEqual => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Identifier(_) => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Str(_) => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Number(_) => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::And => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Class => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Else => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::False => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Fun => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::For => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::If => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Nil => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Or => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Print => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Return => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Super => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::This => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::True => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Var => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::While => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Eof => &ParseRule {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParseError> {
        self.cursor.advance()?;
        let mut rule = Self::get_parsing_rule_for_token(&self.cursor.current()?.kind);
        let can_assign: bool = precedence < Precedence::Assignement;

        // call the rule associated with handling
        // expressing **STARTING** with this token
        let prefix_fn = rule.prefix.ok_or(ParseError::ExpectedExpression)?;
        prefix_fn(self, can_assign)?;

        while precedence <= rule.precedence {
            self.cursor.advance()?;
            // call the rule associated with handling
            // expressing **CONTAINING** this token
            rule = Self::get_parsing_rule_for_token(&self.cursor.previous()?.kind);
            let infix_fn = rule.infix.ok_or(ParseError::ExpectedExpression)?;
            infix_fn(self, can_assign)?;
        }
        if can_assign && self.cursor.matches(TokenKind::Equal)? {
            return Err(ParseError::ExpectedToken(
                "Invalid assignement target.".into(),
            ));
        }
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
