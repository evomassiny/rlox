use lexer::{Span, Token, TokenKind};

/// A Statement
/// (source that don't evaluate to a value)
#[derive(Debug, PartialEq)]
pub struct Stmt {
    /// All statement variants
    pub kind: StmtKind,
    /// position in source string
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind {
    /// List of inner statements
    Block(Vec<Stmt>),
    /// Class name, super class variable name, functions
    Class(String, Option<String>, Vec<Stmt>),
    /// condition, then branch, else branch
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
    /// function name, args, body
    Function(String, Vec<String>, Vec<Stmt>),
    Expr(Box<Expr>),
    Print(Box<Expr>),
    Return(Option<Box<Expr>>),
    /// var name, initializer
    Var(String, Box<Expr>),
    /// condition, body
    While(Box<Expr>, Box<Stmt>),
    /// initializer, condition, increment, body
    For(
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Option<Box<Expr>>,
        Box<Stmt>,
    ),
}

/// an Expression
/// (source that evaluates to a value)
#[derive(Debug, PartialEq)]
pub struct Expr {
    /// All Expression variants
    pub kind: ExprKind,
    /// position in source string
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Literal(LiteralKind),
    /// operator, expression
    Unary(UnaryExprKind, Box<Expr>),
    /// left, (+, -, /, *, ==, !=, >=, >, <=, <), right
    Binary(Box<Expr>, BinaryExprKind, Box<Expr>),
    /// left, ( &&, ||), right
    Logical(Box<Expr>, LogicalExprKind, Box<Expr>),
    /// wrapped expression
    Grouping(Box<Expr>),
    /// callee, parenthesis, Args
    Call(Box<Expr>, Vec<Expr>),
    /// Object, attribute
    Get(Box<Expr>, String),
    /// Object, attribute, value
    Set(Box<Expr>, String, Box<Expr>),
    /// l_value (identifier), r_value
    Assign(String, Box<Expr>),
    /// method
    Super(String),
    /// keyword
    This,
    /// var name
    Variable(String),
}

#[derive(Debug, PartialEq)]
pub enum LiteralKind {
    Num(f64),
    Str(String),
    Bool(bool),
    Nil,
}

#[derive(Debug, PartialEq)]
pub enum BinaryExprKind {
    Add,
    Sub,
    Div,
    Mul,
    NotEqual,
    Equal,
    LessEqual,
    Less,
    GreaterEqual,
    Greater,
}

#[derive(Debug, PartialEq)]
pub enum LogicalExprKind {
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub enum UnaryExprKind {
    Not,
    Minus,
}
