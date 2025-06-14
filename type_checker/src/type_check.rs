use super::TypeTable;
use parser::{
    BinaryExprKind, Expr, ExprKind, LiteralKind, NodeId, Stmt, StmtKind,
    UnaryExprKind,
};
use resolver::{Ast, SymbolId};

#[derive(Debug)]
pub enum TypeError {
    NoSuperClass,
    NotInClass,
}

#[derive(Debug)]
pub struct TypedAst {}

/// Constraints are properties of
/// expression nodes that must be kept when
/// picking the correct type for an expression.
///
/// They are tied to AST nodes, identified by their `NodeId`.
#[derive(Debug)]
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
    // `a = 1` means `a` has the same type as `1`
    SymbolCanHoldTypeOf(SymbolId, NodeId),
    HasTypeOfSymbol(NodeId, SymbolId),
    IsClass(NodeId, SymbolId),
    ClassHasAttr(SymbolId, String),
    HasTypeOfAttr(NodeId, SymbolId, String),
}

struct ConstraintStore {
    pub(crate) constraints: Vec<Constraint>,
    // those are traversal states
    // current class, current super class
    in_class_declaration: Option<(SymbolId, Option<SymbolId>)>,
}
impl ConstraintStore {
    pub fn new() -> Self {
        Self {
            constraints: Vec::new(),
            in_class_declaration: None,
        }
    }

    pub fn get(&self, node_id: NodeId) -> Option<&[Constraint]> {
        todo!()
    }

    pub fn add(&mut self, constraint: Constraint) {
        self.constraints.push(constraint)
    }

    /// Set the class id we are currently traversing
    /// (I don't allow class definition nesting, Bob N. does)
    pub fn set_current_class(&mut self, class: &SymbolId, super_class: &Option<SymbolId>) {
        self.in_class_declaration = Some((*class, *super_class));
    }
    
    /// Return the class ID we are currentlty traversing
    pub fn get_current_class(&self) -> Option<SymbolId> {
        match self.in_class_declaration {
            Some((class_id, _)) => Some(class_id),
            None => None,
        }
    }

    /// Clear current class
    pub fn clear_current_class(&mut self) {
        self.in_class_declaration = None;
    }

    /// Return the ID of the parent class we are currently traversing
    pub fn get_current_super_class(&self) -> Option<SymbolId> {
        match self.in_class_declaration {
            Some((_, Some(super_class_id))) => Some(super_class_id),
            _ => None,
        }
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
            let arg_ids = args.iter().map(|arg| arg.id).collect();
            store.add(IsCallableWith(callee_expr.id, arg_ids));
            // we also know that the call expression
            // has the type of the return value of the expression.
            store.add(ReturnTypeOf(callee_expr.id));
        }
        Assign(bind_name, r_value_expr) => {
            // The assign expression itself is nil
            store.add(IsNil(expr.id));
            // the object can store the type of the `r_value_expr`
            store.add(SymbolCanHoldTypeOf(*bind_name, r_value_expr.id));
        }
        Variable(bind_name) => {
            // The bind expression itself is nil
            store.add(IsNil(expr.id));
            // variable has the type of the r_value
            store.add(HasTypeOfSymbol(expr.id, *bind_name));
        }
        Get(object_expr, attr_name) => {
            todo!()
        }
        Set(object_expr, attr_name, r_value_expr) => todo!(),
        Super(attr_name) => {
            let super_class = store.get_current_super_class()
                .ok_or(TypeError::NoSuperClass)?;
            store.add(IsClass(expr.id, super_class));
            store.add(HasTypeOfAttr(expr.id, super_class, attr_name.clone()));
        }
        This => {
            let this_class = store.get_current_class()
                .ok_or(TypeError::NotInClass)?;
            store.add(IsClass(expr.id, this_class));

        }
    };
    Ok(())
}

fn collect_constraints_in_stmt<'set>(
    stmt: &Stmt<SymbolId>,
    set: &'set mut ConstraintStore,
) -> Result<(), TypeError> {
    use Constraint::*;
    use StmtKind::*;

    match &stmt.kind {
        Block(stmts) => {
            for stmt in stmts {
                let _ = collect_constraints_in_stmt(stmt, set)?;
            }
        }
        Class(name, maybe_super_name, methods) => {
            set.set_current_class(name, maybe_super_name);
            for method in methods {
                let _ = collect_constraints_in_stmt(stmt, set)?;
            }
            set.clear_current_class();
        }
        If(condition, then, maybe_else) => todo!("If stmt"),
        Function(name, args, body) => todo!("Function"),
        Expr(expr) => {
            let _ = collect_expression_constraints(expr, set)?;
        }
        Print(expr) => {
            let _ = collect_expression_constraints(expr, set)?;
        }
        Return(maybe_expr) => todo!("Return"),
        Var(name, intializer) => {
            let _ = collect_expression_constraints(intializer, set)?;
            set.add(SymbolCanHoldTypeOf(*name, intializer.id))
        }
        While(condition, body) => todo!("While loops"),
        For(maybe_initializer, maybe_condition, maybe_increment, body) => {
            todo!("For loops")
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
    for constraint in &constraints.constraints {
        println!("{constraint:?}");
    }
    let types = TypeTable::new();
    todo!()
}
