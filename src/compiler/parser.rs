use super::compiler::Compiler;
use crate::lexer::{LexerError, PeekOffset, Span, Token, TokenKind, Tokenize};
use std::error::Error;

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

pub enum ParseError {
    ExpectedToken(String),
    ScanningError(LexerError),
    ExpectedExpression,
    Starved,
}

fn expression(parser: &mut Parser, _can_assign: bool) -> Result<(), ParseError> {
    Ok(())
}

fn grouping(parser: &mut Parser, can_assign: bool) -> Result<(), ParseError> {
    expression(parser, can_assign)?;
    parser.consume(TokenKind::RightParen, "Expecting closing parenthesis.")
}

pub struct Handler {
    prefix: Option<fn(&mut Parser, bool) -> Result<(), ParseError>>,
    infix: Option<fn(&mut Parser, bool) -> Result<(), ParseError>>,
    precedence: Precedence,
}

fn get_rule(kind: &TokenKind) -> Handler {
    match kind {
        &TokenKind::LeftParen => Handler {
            prefix: Some(grouping),
            infix: None,
            precedence: Precedence::Call,
        },
        &TokenKind::RightParen => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::LeftBrace => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::RightBrace => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Comma => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Dot => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Minus => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Plus => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Semicolon => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Slash => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Star => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Bang => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::BangEqual => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Equal => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::EqualEqual => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Greater => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::GreaterEqual => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Less => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::LessEqual => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Identifier(_) => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Str(_) => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Number(_) => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::And => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Class => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Else => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::False => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Fun => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::For => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::If => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Nil => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Or => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Print => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Return => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Super => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::This => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::True => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Var => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::While => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
        &TokenKind::Eof => Handler {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        },
    }
}

pub struct Parser {
    lexer: Box<dyn Tokenize>,
    /// emits bytecode
    compiler: Compiler,
    current: Option<Token>,
    previous: Option<Token>,
}

impl Parser {
    pub fn new(lexer: Box<dyn Tokenize>) -> Self {
        Self {
            lexer,
            compiler: Compiler::new(),
            current: None,
            previous: None,
        }
    }

    fn advance(&mut self) -> Result<(), ParseError> {
        self.previous = self.current.take();
        self.current = Some(
            self.lexer
                .scan_next()
                .map_err(|e| ParseError::ScanningError(e))?,
        );
        Ok(())
    }
    fn current<'a>(&'a self) -> Result<&'a Token, ParseError> {
        self.current.as_ref().ok_or(ParseError::Starved)
    }
    fn previous<'a>(&'a self) -> Result<&'a Token, ParseError> {
        self.previous.as_ref().ok_or(ParseError::Starved)
    }

    fn consume(&mut self, kind: TokenKind, err_msg: &str) -> Result<(), ParseError> {
        let token = self
            .lexer
            .scan_next()
            .map_err(|e| ParseError::ScanningError(e))?;
        if !matches!(token.kind, kind) {
            return Err(ParseError::ExpectedToken(err_msg.into()));
        }
        Ok(())
    }

    fn check(&self, kind: TokenKind) -> Result<bool, ParseError> {
        matches!(kind, self.current()?.kind)
    }

    fn matches(&mut self, kind: TokenKind) -> Result<bool, ParseError> {
        if !self.check(kind)? {
            return Ok(false);
        }
        let _ = self.advance()?;
        Ok(true)
    }

    pub fn parsePrecedence(&mut self) -> Result<(), ParseError> {
        let _ = self.advance()?;
        let mut rule = get_rule(&self.current()?.kind);
        let precedence = rule.precedence;
        let can_assign: bool = rule.precedence < Precedence::Assignement;

        // call the rule associated with handling
        // expressing **STARTING** with this token
        let prefix_fn = rule.prefix.ok_or(ParseError::ExpectedExpression)?;
        prefix_fn(self, can_assign)?;

        while precedence <= rule.precedence {
            let _ = self.advance()?;
            // call the rule associated with handling
            // expressing **CONTAINING** this token
            rule = get_rule(&self.previous()?.kind);
            let infix_fn = rule.infix.ok_or(ParseError::ExpectedExpression)?;
            infix_fn(self, can_assign)?;
        }
        if can_assign && self.matches(TokenKind::Equal)? {
            return Err(ParseError::ExpectedToken(
                "Invalid assignement target.".into(),
            ));
        }
        Ok(())
    }
}
