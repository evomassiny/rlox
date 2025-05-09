use super::TypeTable;
use parser::{
    BinaryExprKind, Expr, ExprKind, LiteralKind, NodeId, Stmt, StmtKind,
    UnaryExprKind,
};
use resolver::{Ast, SymbolId};

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

/// Constraints are properties of
/// expression nodes that must be kept when
/// picking the correct type for an expression.
///
/// They are tied to AST nodes, identified by their `NodeId`.
pub enum Constraint {
    IsNumOrStr(NodeId),
    IsNum(NodeId),
    IsStr(NodeId),
    IsBool(NodeId),
    IsNil(NodeId),
    // a function callable with types equivalent at those
    IsCallableWith(NodeId, Vec<NodeId>),
    // Exact match
    Same(NodeId, NodeId),
    // equivalent to
    Equivalent(NodeId, NodeId),
    // handle `a = b or c`,
    // `a` can have both type depending of the runtime value
    IsEither(NodeId, NodeId, NodeId),
    ReturnTypeOf(NodeId),
    //IsClass(SymbolId),
    //HasAttr(String),
}

struct ConstraintStore {
    constraints: Vec<Constraint>,
}
impl ConstraintStore {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
        }
    }

    pub fn get(&self, node_id: NodeId) -> Option<&[Constraint]> {
        todo!()
    }

    pub fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint)
    }
}

fn collect_binary_expression_constraints(
    binary_id: NodeId,
    left: &Expr<SymbolId>,
    kind: &BinaryExprKind,
    right: &Expr<SymbolId>,
    store: &mut ConstraintStore,
) -> Result<(), TypeError> {
    use BinaryExprKind::*;
    use Constraint::*;

    match kind {
        // both operands can only be either strings or numbers
        // _and_ are equivalent
        Add => {
            store.add(Same(left.id, right.id));
            store.add(IsNumOrStr(left.id));
            store.add(Same(binary_id, left.id));
        }
        // both operands are numbers
        LessEqual | Less | GreaterEqual | Greater | Sub | Div | Mul => {
            store.add(IsNum(left.id));
            store.add(IsNum(right.id));
            store.add(IsBool(binary_id));
        }
        // both operands are equivalents
        NotEqual | Equal => {
            store.add(Same(left.id, right.id));
            store.add(IsBool(binary_id));
        }
    }
    Ok(())
}

fn collect_expression_constraints<'set>(
    expr: &Expr<SymbolId>,
    store: &'set mut ConstraintStore,
) -> Result<(), TypeError> {
    // TODO:
    // fill this !
    //
    use Constraint::*;
    use ExprKind::*;
    match &expr.kind {
        // for str, number and bools,
        // we directly know the type of the expression
        // for Nil, we can't decide anything.
        Literal(literal_kind) => {
            match &literal_kind {
                LiteralKind::Num(..) => store.add(IsNum(expr.id)),
                LiteralKind::Str(..) => store.add(IsStr(expr.id)),
                LiteralKind::Bool(..) => store.add(IsBool(expr.id)),
                LiteralKind::Nil => {}
            };
        }
        // 2 cases:
        // * `!exp`: exp can be evaluated as a boolean, but anything can.
        // * `-exp`: `-exp` is a number, so is `exp`
        Unary(kind, inner_expr) => {
            match &kind {
                UnaryExprKind::Not => {
                    // the `!exp` is a boolean
                    // anything can be casted as a boolean
                    // so it doesn't give us any more information
                    store.add(IsBool(expr.id));
                }
                UnaryExprKind::Minus => {
                    // both the current expression and the innner one are numbers
                    store.add(IsNum(expr.id));
                    store.add(IsNum(inner_expr.id));
                }
            };
        }
        Binary(left, kind, right) => collect_binary_expression_constraints(
            expr.id, left, kind, right, store,
        )?,
        Logical(left, _kind, right) => {
            // the type of the binary expression itself depends of the evaluation,
            store.add(IsEither(expr.id, left.id, right.id));
        }
        Grouping(inner_expr) => {
            // the inner node has the exact same type as its child
            store.add(Same(expr.id, inner_expr.id));
        }
        Call(callee_expr, args) => {
            // We known that "callee_expr" is a function.
            // which should be callable with those args
            let arg_ids = args.iter().map(|arg| arg.id ).collect();
            store.add(IsCallableWith(callee_expr.id, arg_ids));
            // we also know that the call expression
            // has the type of the return value of the expression.
            store.add(ReturnTypeOf(callee_expr.id));
        },
        Assign(bind_name, r_value_expr) => {
            // What should we do here ?
            // if `bind_name` is a global, it could be redifined

        },
        Variable(bind_name) => todo!(),
        Get(object_expr, attr_name) => todo!(),
        Set(object_expr, attr_name, r_value_expr) => todo!(),
        Super(attr_name) => todo!(),
        This => todo!(),
    };
    Ok(())
}

fn collect_constraints_in_stmt<'set>(
    stmt: &Stmt<SymbolId>,
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
        For(maybe_initializer, maybe_condition, maybe_increment, body) => {
            todo!()
        }
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
