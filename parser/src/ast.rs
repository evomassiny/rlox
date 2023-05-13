/// This file defines all the nodes of an Abstrat syntax Tree:
/// a tree of statements, and expressions.
///
/// All statements (`Stmt`s) and expressions (`Expr`s) contain a reference
/// to the source that defined them: a `Span`.
///
/// All those structs are generic over the type of theirs symbols,
/// (variable bindings names), in the first stage of the compiler,
/// we use simple `String`s, after performing the name resolution,
/// we switch to references to a dedicated type (see `resolver::symbols`).
use lexer::Span;

/// A Statement
/// (source that don't evaluate to a value)
#[derive(Debug, PartialEq)]
pub struct Stmt<Symbol> {
    /// All statement variants
    pub kind: StmtKind<Symbol>,
    /// position in source string
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum StmtKind<Symbol> {
    /// List of inner statements
    Block(Vec<Stmt<Symbol>>),
    /// Class name, super class variable name, functions
    Class(Symbol, Option<Symbol>, Vec<Stmt<Symbol>>),
    /// condition, then branch, else branch
    If(
        Box<Expr<Symbol>>,
        Box<Stmt<Symbol>>,
        Option<Box<Stmt<Symbol>>>,
    ),
    /// function name, args, body
    Function(Symbol, Vec<Symbol>, Vec<Stmt<Symbol>>),
    Expr(Box<Expr<Symbol>>),
    Print(Box<Expr<Symbol>>),
    Return(Option<Box<Expr<Symbol>>>),
    /// var name, initializer
    Var(Symbol, Box<Expr<Symbol>>),
    /// condition, body
    While(Box<Expr<Symbol>>, Box<Stmt<Symbol>>),
    /// initializer, condition, increment, body
    For(
        Option<Box<Expr<Symbol>>>,
        Option<Box<Expr<Symbol>>>,
        Option<Box<Expr<Symbol>>>,
        Box<Stmt<Symbol>>,
    ),
}

/// an Expression
/// (source that evaluates to a value)
#[derive(Debug, PartialEq)]
pub struct Expr<Symbol> {
    /// All Expression variants
    pub kind: ExprKind<Symbol>,
    /// position in source string
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum ExprKind<Symbol> {
    Literal(LiteralKind),
    /// operator, expression
    Unary(UnaryExprKind, Box<Expr<Symbol>>),
    /// left, (+, -, /, *, ==, !=, >=, >, <=, <), right
    Binary(Box<Expr<Symbol>>, BinaryExprKind, Box<Expr<Symbol>>),
    /// left, ( &&, ||), right
    Logical(Box<Expr<Symbol>>, LogicalExprKind, Box<Expr<Symbol>>),
    /// wrapped expression
    Grouping(Box<Expr<Symbol>>),
    /// callee, parenthesis, Args
    Call(Box<Expr<Symbol>>, Vec<Expr<Symbol>>),
    /// Object, attribute
    Get(Box<Expr<Symbol>>, Symbol),
    /// Object, attribute, value
    Set(Box<Expr<Symbol>>, Symbol, Box<Expr<Symbol>>),
    /// l_value (identifier), r_value
    Assign(Symbol, Box<Expr<Symbol>>),
    /// method
    Super(Symbol),
    /// keyword
    This,
    /// var name
    Variable(Symbol),
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
