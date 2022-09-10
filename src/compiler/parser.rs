use crate::lexer::{Token, TokenKind, LexerError, Span, PeekOffset, Lexer};
use super::compiler::Compiler;
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


pub struct Parser<'a, T> {
    lexer: &'a mut Lexer<T>,
    /// emits bytecode
    compiler: Compiler,
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
}

impl <'a, T: PeekOffset> Parser<'a, T> {

    pub fn new(lexer: &'a mut Lexer<T>) -> Self {
        Self {
            lexer,
            compiler: Compiler::new(),
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
        }
    }

    fn advance(&mut self) -> Result<(), LexerError> {
        self.previous = self.current.take();
        self.current = Some(self.lexer.scan_next()?);
        Ok(())
    }

    /// Parse the start of statement/expression
    fn parse_prefix(&mut self, kind: TokenKind, can_assign: bool) -> Result<(), ()> {
        // mimic the first element of the `ParseRule` C-struct
        match kind {
            TokenKind::LeftParen=> todo!(),
            TokenKind::RightParen=> todo!(),
            TokenKind::LeftBrace=> todo!(),
            TokenKind::RightBrace=> todo!(),
            TokenKind::Comma=> todo!(),
            TokenKind::Dot=> todo!(),
            TokenKind::Minus=> todo!(),
            TokenKind::Plus=> todo!(),
            TokenKind::Semicolon=> todo!(),
            TokenKind::Slash=> todo!(),
            TokenKind::Star=> todo!(),
            TokenKind::Bang=> todo!(),
            TokenKind::BangEqual=> todo!(),
            TokenKind::Equal=> todo!(),
            TokenKind::EqualEqual=> todo!(),
            TokenKind::Greater=> todo!(),
            TokenKind::GreaterEqual=> todo!(),
            TokenKind::Less=> todo!(),
            TokenKind::LessEqual=> todo!(),
            TokenKind::Identifier(_)=> todo!(),
            TokenKind::Str(_)=> todo!(),
            TokenKind::Number(_)=> todo!(),
            TokenKind::And=> todo!(),
            TokenKind::Class=> todo!(),
            TokenKind::Else=> todo!(),
            TokenKind::False=> todo!(),
            TokenKind::Fun=> todo!(),
            TokenKind::For=> todo!(),
            TokenKind::If=> todo!(),
            TokenKind::Nil=> todo!(),
            TokenKind::Or=> todo!(),
            TokenKind::Print=> todo!(),
            TokenKind::Return=> todo!(),
            TokenKind::Super=> todo!(),
            TokenKind::This=> todo!(),
            TokenKind::True=> todo!(),
            TokenKind::Var=> todo!(),
            TokenKind::While=> todo!(),
            TokenKind::Eof=> todo!(),
        }
        Ok(())
    }

}

/*
/**
 * Parse all expression until we reach a token associated
 * with an higher precedence than `precedence`.
 *
 * Assumes that the first token is the starting point of a
 * prefix expression.
 */
static void parsePrecendence(Precedence precedence) {
  advance();
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  bool canAssign = precedence <= PREC_ASSIGNMENT;
  // call the rule associated with handling
  // expressing **STARTING** with this token
  prefixRule(canAssign);

  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    // call the rule associated with handling
    // expressing **CONTAINING** this token
    infixRule(canAssign);
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignement target.");
  }
}
*/
