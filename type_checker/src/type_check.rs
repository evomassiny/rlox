use super::TypeTable;
use parser::{Expr, ExprKind, LiteralKind, NodeId, Stmt, StmtKind, BinaryExprKind, UnaryExprKind, LogicalExprKind};
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
    IsNumOrStr,
    IsNum,
    IsStr,
    IsBool,
    IsNil,
    // a function callable with types equivalent at those
    IsCallableWith(Box<[NodeId]>),
    // Exect match
    SameAs(NodeId),
    // equivalent to
    EqTo(NodeId),
    // handle `a = b or c`, 
    // `a` can have both type depending of the runtime value
    IsEither(NodeId, NodeId),
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
        Self {
            constraints: BTreeMap::new(),
        }
    }

    pub fn get(&self, node_id: NodeId) -> Option<&[Constraint]> {
        None
    }

    pub fn add(&mut self, node_id: NodeId, constraint: Constraint) {
        let mut set = self.constraints.entry(node_id).or_insert_with(Vec::new);
        set.push(constraint)
    }
}

fn collect_binary_expression_constraints(
    left: &Expr<Sym>,
    kind: &BinaryExprKind,
    right: &Expr<Sym>,
    store: &mut ConstraintStore,
) -> Result<(), TypeError> {
    use BinaryExprKind::*;
    use Constraint::*;

    match kind {
        // both operands can only be either strings or numbers
        // _and_ are equivalent
        Add => {
            store.add(left.id, EqTo(right.id));
            store.add(right.id, EqTo(left.id));
            store.add(left.id, IsNumOrStr);
            store.add(right.id, IsNumOrStr);
        }
        // both operands are numbers
        LessEqual | Less | GreaterEqual | Greater | Sub | Div | Mul => {
            store.add(left.id, IsNum);
            store.add(right.id, IsNum);
        }
        // both operands are equivalents
        NotEqual | Equal => {
            store.add(left.id, EqTo(right.id));
            store.add(right.id, EqTo(left.id));
        }
    }
    Ok(())
}

fn collect_expression_constraints<'set>(
    expr: &Expr<Sym>,
    store: &'set mut ConstraintStore,
) -> Result<(), TypeError> {
    // TODO:
    // fill this !
    //
    use Constraint::*;
    use ExprKind::*;
    match &expr.kind {
        // for str, number and bools, that's the easy case, we diretly know the type of the expression
        // for Nil, we can't decide anything.
        Literal(literal_kind) => {
            match &literal_kind {
                LiteralKind::Num(..) => store.add(expr.id, IsNum),
                LiteralKind::Str(..) => store.add(expr.id, IsStr),
                LiteralKind::Bool(..) => store.add(expr.id, IsBool),
                LiteralKind::Nil => {}
            };
        }
        // 2 cases:
        // * `!exp`: exp can be evaluated as a boolean
        // * `-exp`: exp can be evaluated as a number
        Unary(kind, inner_expr) => {
            match &kind {
                UnaryExprKind::Not => {
                    // the `!exp` is a boolean
                    store.add(expr.id, IsBool);
                    // anything can be casted as a boolean
                    // so it doesn't give us any more information
                }
                UnaryExprKind::Minus => {
                    // both the current expression and the innner one are numbers
                    store.add(expr.id, IsNum);
                    store.add(inner_expr.id, IsNum);
                }
            };
        }
        Binary(left, kind, right) => {
            collect_binary_expression_constraints(left, kind, right, store)?
        }
        Logical(left, _kind, right) => {
            // the type of the binary expression itself depends ?
            // are ternaries expressions allowed ?
            store.add(expr.id, IsEither(left.id, right.id));
        }
        Grouping(inner_expr) => {
            // the inner node as the exact same type as its child
            store.add(expr.id, SameAs(inner_expr.id)); 
        }
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
    use Constraint::*;
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
