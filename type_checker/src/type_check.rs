use super::TypeTable;
use parser::{Expr, ExprKind, Stmt, StmtKind, NodeId, LiteralKind};
use resolver::{Ast, Sym, SymbolId};

use std::collections::BTreeMap;


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
    IsNum,
    IsStr,
    IsBool,
    IsNil,
    // equivalent to
    EqTo(SymbolId),
    ReturnTypeOf(SymbolId),
    IsClass(SymbolId),
    HasAttr(String),
}

struct ConstraintStore {
    // node ids are sorted, and contiguous node ids are often 
    // read together so i expect that a btreemap is a nice fit for this
    // read/write pattern.
    // If performance somehow becomes an issue, we could use a flat array.
    constraints: BTreeMap<NodeId, Vec<Constraint>>,
}
impl ConstraintStore {

    pub fn new() -> Self {
        Self{ constraints: BTreeMap::new() }
    }

    pub fn get(&self, node_id: NodeId) -> Option<&[Constraint]> {
        None
    }

    pub fn add(&mut self, node_id: NodeId, constraint: Constraint) {
        let mut set = self.constraints.entry(node_id).or_insert_with(Vec::new);
        set.push(constraint)
    }

}


fn collect_expression_constraints<'set>(
    expr: &Expr<Sym>,
    store: &'set mut ConstraintStore,
) -> Result<(), TypeError> {

    // TODO:
    // fill this ! 
    //
    use ExprKind::*;
    match &expr.kind {
        Literal(literal_kind) => {
            use LiteralKind::*;
            let constraint = match &literal_kind {
                Num(..) => Constraint::IsNum,
                Str(..) => Constraint::IsStr,
                Bool(..) => Constraint::IsBool,
                Nil => Constraint::IsNil,
            };
            store.add(&expr.id, constraint);
        },
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
    set: &'set mut ConstraintStore,
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

fn collect_constraints(ast: &Ast) -> Result<ConstraintStore, TypeError> {

    let mut contraints_by_ast_node: ConstraintStore = ConstraintStore::new();

    // traverse the ast and append constraints to each type
    for stmt in &ast.roots {
        collect_constraints_in_stmt(stmt, &mut contraints_by_ast_node)?;
    }

    Ok(contraints_by_ast_node)
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
