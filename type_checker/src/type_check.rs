use super::TypeTable;
use parser::{Expr, ExprKind, Stmt, StmtKind};
use resolver::{Ast, Sym, SymbolId};

#[derive(Debug)]
pub enum TypeError {}

#[derive(Debug)]
pub struct TypedAst {}

pub enum MathOp {
    // float |  string
    Add,
    // float
    Sub,
    // float
    Mul,
    // float
    Div,
}

pub enum Constraint {
    CanMathWith(MathOp, SymbolId),
    IsNumber,
    IsString,
    // equivalent to
    EqTo(SymbolId),
    ReturnTypeOf(SymbolId),
    IsClass(SymbolId),
    HasAttr(String),
}

pub struct ConstraintSet(Vec<Constraint>);
impl ConstraintSet {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

fn collect_expression_constraints<'set>(
    expr: &Expr<Sym>,
    set: &'set mut Vec<ConstraintSet>,
) -> Result<(), TypeError> {

    use ExprKind::*;
    match &expr.kind {
        Literal(literal_kind) => todo!(),
        Unary(kind, inner_expr) => todo!(),
        Binary(left_expr, kind, right_expr) => todo!(),
        Logical(left_expr, kind, right_expr) => todo!(),
        Grouping(inner_expr) => collect_expression_constraints(inner_expr, set),
        Call(callee_expr, args) => todo!(),
        Assign(bind_name, r_value_expr) => todo!(),
        Variable(bind_name) => todo!(),
        Get(object_expr, attr_name) => todo!(),
        Set(object_expr, attr_name, r_value_expr) => todo!(),
        Super(attr_name) => todo!(),
        This => todo!(),
    };
    Ok(())
}

fn collect_constraints_in_stmt<'set>(
    stmt: &Stmt<Sym>,
    set: &'set mut Vec<ConstraintSet>,
) -> Result<(), TypeError> {

    use StmtKind::*;
    match &stmt.kind {
        Block(stmts) => todo!(),
        Class(name, maybe_super_name, methods) => todo!(),
        If(condition, then, maybe_else) => todo!(),
        Function(name, args, body) => todo!(),
        Expr(expr) => collect_expression_constraints(expr, set),
        Print(expr) => todo!(),
        Return(maybe_expr) => todo!(),
        Var(name, intializer) => todo!(),
        While(condition, body) => todo!(),
        For(maybe_initializer, maybe_condition, maybe_increment, body) => todo!(),
    };
    Ok(())
}

fn collect_constraints(ast: &Ast) -> Result<Vec<ConstraintSet>, TypeError> {
    // build a table a list of constraints, per variable symbol
    let mut contraints_by_symbol: Vec<ConstraintSet> = Vec::with_capacity(ast.symbols.len());
    for _ in 0..ast.symbols.len() {
        contraints_by_symbol.push(ConstraintSet::new());
    }

    // traverse the ast and append constraints to each type
    for stmt in &ast.roots {
        collect_constraints_in_stmt(stmt, &mut contraints_by_symbol)?;
    }

    Ok(contraints_by_symbol)
}

pub fn type_check(untyped_ast: Ast) -> Result<TypedAst, TypeError> {
    // done in two part:
    // * first we collect a bunch of constraints for each type,
    //   for instance `a + c` means `a` and `c` are of the same type,
    //   and this type implements `add`
    // * then we actually solve thoses constraints
    let constraints = collect_constraints(&untyped_ast)?;
    let types = TypeTable::new();
    todo!()
}
