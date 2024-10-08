use resolver::{Ast, Symbol, SymbolId, SymbolTable, Sym};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};
use super::{Type, TypeId, TypeTable, TypeConstraint};

#[derive(Debug)]
pub enum TypeError { }

#[derive(Debug)]
pub struct TypedAst { }


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

fn collect_constraints_in_stmt<'set>(stmt: &Stmt<Sym>, set: &'set mut Vec<ConstraintSet>) -> Result<(), TypeError> {
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
    let mut types = TypeTable::new();
    todo!()
}

