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

/// Facts are properties of
/// expression nodes that must be kept when
/// picking the correct type for an expression.
///
/// They are tied to AST nodes, identified by their `NodeId`.
#[derive(Debug)]
pub enum Fact {
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
    // Expression (0) as the type of the attribute (2)
    // from the expression (1)
    HasTypeOfObjAttr(NodeId, NodeId, String),
    IsClass(NodeId, SymbolId),
    IsObjectWithAttr(NodeId,  String),
    ExprHasAttr(NodeId, String),
    HasTypeOfClassAttr(NodeId, SymbolId, String),
}

struct TypeSolver {
    pub(crate) facts: Vec<Fact>,
    // those are traversal states
    // current class, current super class
    in_class_declaration: Option<(SymbolId, Option<SymbolId>)>,
}
impl TypeSolver {
    pub fn new() -> Self {
        Self {
            facts: Vec::new(),
            in_class_declaration: None,
        }
    }

    pub fn get(&self, node_id: NodeId) -> Option<&[Fact]> {
        todo!()
    }

    /// add a constraint to the solver
    pub fn add_fact(&mut self, constraint: Fact) {
        self.facts.push(constraint)
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

/// given a binary expression, 
/// collect the type facts derived from
/// their usage.
fn collect_binary_expression_facts(
    binary_id: NodeId,
    left: &Expr<SymbolId>,
    kind: &BinaryExprKind,
    right: &Expr<SymbolId>,
    solver: &mut TypeSolver,
) -> Result<(), TypeError> {
    use BinaryExprKind::*;
    use Fact::*;

    // recursively collect facts
    let _ = collect_expression_facts(left, solver)?;
    let _ = collect_expression_facts(right, solver)?;

    match kind {
        // both operands can only be either strings or numbers
        // _and_ are equivalent
        Add => {
            solver.add_fact(Same(left.id, right.id));
            solver.add_fact(IsNumOrStr(left.id));
            solver.add_fact(Same(binary_id, left.id));
        }
        // both operands are numbers
        LessEqual | Less | GreaterEqual | Greater | Sub | Div | Mul => {
            solver.add_fact(IsNum(left.id));
            solver.add_fact(IsNum(right.id));
            solver.add_fact(IsBool(binary_id));
        }
        // both operands are equivalents
        NotEqual | Equal => {
            solver.add_fact(Same(left.id, right.id));
            solver.add_fact(IsBool(binary_id));
        }
    }
    Ok(())
}

fn collect_expression_facts<'set>(
    expr: &Expr<SymbolId>,
    solver: &'set mut TypeSolver,
) -> Result<(), TypeError> {
    use Fact::*;
    use ExprKind::*;
    match &expr.kind {
        // for str, number and bools,
        // we directly know the type of the expression
        // for Nil, we can't decide anything.
        Literal(literal_kind) => {
            match &literal_kind {
                LiteralKind::Num(..) => solver.add_fact(IsNum(expr.id)),
                LiteralKind::Str(..) => solver.add_fact(IsStr(expr.id)),
                LiteralKind::Bool(..) => solver.add_fact(IsBool(expr.id)),
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
                    solver.add_fact(IsBool(expr.id));
                }
                UnaryExprKind::Minus => {
                    // both the current expression and the innner one are numbers
                    solver.add_fact(IsNum(expr.id));
                    solver.add_fact(IsNum(inner_expr.id));
                }
            };
            let _ = collect_expression_facts(inner_expr, solver)?;
        }
        Binary(left, kind, right) => collect_binary_expression_facts(
            expr.id, left, kind, right, solver,
        )?,
        Logical(left, _kind, right) => {
            // the type of the binary expression itself depends of the evaluation,
            solver.add_fact(IsEither(expr.id, left.id, right.id));
            let _ = collect_expression_facts(left, solver)?;
            let _ = collect_expression_facts(right, solver)?;
        }
        Grouping(inner_expr) => {
            // the inner node has the exact same type as its child
            solver.add_fact(Same(expr.id, inner_expr.id));
            let _ = collect_expression_facts(inner_expr, solver)?;
        }
        Call(callee_expr, args) => {
            // We known that "callee_expr" is a function.
            // which should be callable with those args
            let arg_ids = args.iter().map(|arg| arg.id).collect();
            solver.add_fact(IsCallableWith(callee_expr.id, arg_ids));
            // we also know that the call expression
            // has the type of the return value of the expression.
            solver.add_fact(ReturnTypeOf(callee_expr.id));

            let _ = collect_expression_facts(callee_expr, solver)?;
            for arg_expr in args {
                let _ = collect_expression_facts(arg_expr, solver)?;
            }
        }
        Assign(bind_name, r_value_expr) => {
            // The assign expression itself is nil
            solver.add_fact(IsNil(expr.id));
            // the object can store the type of the `r_value_expr`
            solver.add_fact(SymbolCanHoldTypeOf(*bind_name, r_value_expr.id));
            let _ = collect_expression_facts(r_value_expr, solver)?;
        }
        Variable(bind_name) => {
            // The bind expression itself is nil
            solver.add_fact(IsNil(expr.id));
            // variable has the type of the r_value
            solver.add_fact(HasTypeOfSymbol(expr.id, *bind_name));
        }
        Get(object_expr, attr_name) => {
            solver.add_fact(IsObjectWithAttr(object_expr.id, attr_name.clone()));
            let _ = collect_expression_facts(object_expr, solver)?;
            solver.add_fact(HasTypeOfObjAttr(expr.id, object_expr.id, attr_name.clone()));
        }
        Set(object_expr, attr_name, r_value_expr) => {
            solver.add_fact(IsObjectWithAttr(object_expr.id, attr_name.clone()));
            let _ = collect_expression_facts(object_expr, solver)?;
            let _ = collect_expression_facts(r_value_expr, solver)?;
        },
        Super(attr_name) => {
            let super_class = solver.get_current_super_class()
                .ok_or(TypeError::NoSuperClass)?;
            solver.add_fact(IsClass(expr.id, super_class));
            solver.add_fact(HasTypeOfClassAttr(expr.id, super_class, attr_name.clone()));
        }
        This => {
            let this_class = solver.get_current_class()
                .ok_or(TypeError::NotInClass)?;
            solver.add_fact(IsClass(expr.id, this_class));
        }
    };
    Ok(())
}

fn collect_facts_in_stmt<'set>(
    stmt: &Stmt<SymbolId>,
    set: &'set mut TypeSolver,
) -> Result<(), TypeError> {
    use Fact::*;
    use StmtKind::*;

    match &stmt.kind {
        Block(stmts) => {
            for stmt in stmts {
                let _ = collect_facts_in_stmt(stmt, set)?;
            }
        }
        Class(name, maybe_super_name, methods) => {
            set.set_current_class(name, maybe_super_name);
            for method in methods {
                let _ = collect_facts_in_stmt(stmt, set)?;
            }
            set.clear_current_class();
        }
        If(condition, then, maybe_else) => todo!("If stmt"),
        Function(name, args, body) => todo!("Function"),
        Expr(expr) => {
            let _ = collect_expression_facts(expr, set)?;
        }
        Print(expr) => {
            let _ = collect_expression_facts(expr, set)?;
        }
        Return(maybe_expr) => todo!("Return"),
        Var(name, intializer) => {
            let _ = collect_expression_facts(intializer, set)?;
            set.add_fact(SymbolCanHoldTypeOf(*name, intializer.id))
        }
        While(condition, body) => todo!("While loops"),
        For(maybe_initializer, maybe_condition, maybe_increment, body) => {
            todo!("For loops")
        }
    };
    Ok(())
}

fn collect_facts(ast: &Ast) -> Result<TypeSolver, TypeError> {
    let mut contraints_by_ast_node: TypeSolver = TypeSolver::new();

    // traverse the ast and append facts to each type
    for stmt in &ast.roots {
        collect_facts_in_stmt(stmt, &mut contraints_by_ast_node)?;
    }

    Ok(contraints_by_ast_node)
}

pub fn type_check(untyped_ast: Ast) -> Result<TypedAst, TypeError> {
    // done in two part:
    // * first we collect a bunch of facts for each type,
    //   for instance `a + c` means `a` and `c` are of the same type,
    //   and this type implements `add`
    // * then we actually solve thoses facts
    let facts = collect_facts(&untyped_ast)?;
    for constraint in &facts.facts {
        println!("{constraint:?}");
    }
    let types = TypeTable::new();
    todo!()
}
