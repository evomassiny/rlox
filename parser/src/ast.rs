use lexer::{Span, Token, TokenKind};

/// A Statement
/// (source that don't evaluate to a value)
pub struct Stmt {
    /// All statement variants
    kind: StmtKind,
    /// position in source string
    span: Span,
}

pub enum StmtKind {
    /// List of inner statements
    Block(Vec<Stmt>),
    /// Class name, super class variable, functions
    Class(TokenKind, Option<Box<Expr>>, Vec<Stmt>),
    /// condition, then branch, else branch
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    /// function name, args, body
    Function(String, Vec<Token>, Vec<Stmt>),
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Return(Option<Box<Expr>>),
    /// var name, initializer
    Var(String, Box<Expr>),
    /// condition, body
    While(Box<Expr>, Box<Stmt>),
}

/// an Expression
/// (source that evaluates to a value)
pub struct Expr {
    /// All Expression variants
    kind: ExprKind,
    /// position in source string
    span: Span,
}

pub enum ExprKind {
    Literal(LiteralKind),
    /// operator, expression
    Unary(Token, Box<Expr>),
    /// left, operand, right
    Binary(Box<Expr>, Token, Box<Expr>),
    /// wrapped expression
    Grouping(Box<Expr>),
    /// callee, parenthesis, Args
    Call(Box<Expr>, Token, Vec<Expr>),
    /// Object, attribute
    Get(Box<Expr>, Token),
    /// Object, attribute, value
    Set(Box<Expr>, Token, Box<Expr>),
    /// left hand side, right hand side
    Assign(Token, Box<Expr>),
    /// keyword, method
    Super(Token, Token),
    /// keyword
    This(Token),
    /// var name
    Variable(Token),
}

pub enum LiteralKind {
    Num(f64),
    Str(String),
    Bool(bool),
    Nil,
}
