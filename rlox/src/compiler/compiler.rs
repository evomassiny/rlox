use super::compile_unit::{CompileUnit, ObjFunction};
use super::cursor::{Cursor, ParseError};
/// rlox is a single pass compiler,
/// eg: the parsing and compiling are done in one go.
/// The `Compiler`struct does both.
use crate::lexer::{TokenKind, Tokenize};

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

pub struct Handler {
    prefix: Option<fn(&mut Compiler, bool) -> Result<(), ParseError>>,
    infix: Option<fn(&mut Compiler, bool) -> Result<(), ParseError>>,
    precedence: Precedence,
}

pub struct Compiler {
    cursor: Cursor,
    compile_units: Vec<CompileUnit>,
}

impl Compiler {
    pub fn new(lexer: Box<dyn Tokenize>) -> Self {
        let cursor = Cursor::new(lexer);
        Self {
            cursor,
            compile_units: Vec::new(),
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
    fn get_parsing_rule_for_token(kind: &TokenKind) -> &'static Handler {
        match kind {
            &TokenKind::LeftParen => &Handler {
                prefix: Some(Self::grouping),
                infix: None,
                precedence: Precedence::Call,
            },
            &TokenKind::RightParen => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::LeftBrace => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::RightBrace => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Comma => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Dot => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Minus => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Plus => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Semicolon => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Slash => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Star => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Bang => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::BangEqual => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Equal => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::EqualEqual => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Greater => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::GreaterEqual => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Less => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::LessEqual => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Identifier(_) => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Str(_) => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Number(_) => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::And => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Class => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Else => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::False => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Fun => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::For => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::If => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Nil => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Or => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Print => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Return => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Super => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::This => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::True => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Var => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::While => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
            &TokenKind::Eof => &Handler {
                prefix: None,
                infix: None,
                precedence: Precedence::None,
            },
        }
    }

    pub fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParseError> {
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

    pub fn compile(&mut self) -> Result<ObjFunction, ParseError> {
        self.cursor.advance()?;
        self.compile_units.push(CompileUnit::new_script());

        while !self.cursor.matches(TokenKind::Eof)? {
            // parse declaration, forward
            // errors (if any) and move the token cursor to the next
            // statement. (allow recovery in REPL environment)
            if let Err(e) = self.declaration() {
                self.cursor.move_to_next_stmt();
                return Err(e);
            }
        }

        let unit = self
            .compile_units
            .pop()
            .ok_or(ParseError::CompilationError)?;
        Ok(unit.function)
    }
}
