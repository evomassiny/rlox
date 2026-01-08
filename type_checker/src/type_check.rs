use super::TypeTable;
use parser::{
    BinaryExprKind, Expr, ExprKind, LiteralKind, NodeId, Stmt, StmtKind,
    UnaryExprKind,
};
use resolver::{Ast, SymbolId};
use std::collections::HashMap;

#[derive(Debug)]
pub enum TypeError {
    NoSuperClass,
    NotInClass,
    NotSame(NodeId, NodeId),
    /// the node should be of type `ConcreteType`
    ShouldBe(NodeId, ConcreteType),
}

#[derive(Debug)]
pub struct TypedAst {}

/// Facts are properties of
/// expression nodes that must be kept when
/// solving the type of an expression.
///
/// They are tied to AST nodes, identified by their `NodeId`.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Fact {
    IsNum(NodeId),
    IsStr(NodeId),
    IsBool(NodeId),
    IsNil(NodeId),
    IsClass(NodeId, SymbolId),
    // a function callable with types equivalent at those
    IsCallableWith(NodeId, Vec<NodeId>),
    // Exact match
    Same(NodeId, NodeId),
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
    IsObjectWithAttr(NodeId, String),
    ExprHasAttr(NodeId, String),
    HasTypeOfClassAttr(NodeId, SymbolId, String),
    EitherNumOrStr(NodeId),
}

/// Solved types
#[derive(Clone, Eq, PartialEq, Debug, Hash)]
enum ConcreteType {
    Str,
    Num,
    Bool,
    Nil,
    Callable {
        args_ty: Vec<ConcreteType>,
        return_ty: Box<ConcreteType>,
    },
    Class(SymbolId),
    Union(Vec<ConcreteType>),
}

struct SolvedTypes {
    /// types that we know for sure
    solved_nodes: HashMap<NodeId, ConcreteType>,
    solved_symbol: HashMap<SymbolId, ConcreteType>,
}
impl SolvedTypes {
    pub fn new() -> Self {
        Self {
            solved_nodes: HashMap::new(),
            solved_symbol: HashMap::new(),
        }
    }

    /// add a fact, derived from the user source,
    /// check that it's coherent with the previous facts we kwown,
    /// and use this new information to derive new knowledges
    pub fn use_fact(&mut self, fact: &Fact) -> Result<bool, TypeError> {
        use Fact::*;
        let mut consumed = true;
        match *fact {
            // set `node` as Num
            IsNum(node) => {
                let _ =
                    self.check_or_set_type_for_node(node, ConcreteType::Num)?;
            }
            // set `node` as Str
            IsStr(node) => {
                let _ =
                    self.check_or_set_type_for_node(node, ConcreteType::Str)?;
            }
            // set `node` as bool
            IsBool(node) => {
                let _ =
                    self.check_or_set_type_for_node(node, ConcreteType::Bool)?;
            }
            // set `node` as Nil
            IsNil(node) => {
                let _ =
                    self.check_or_set_type_for_node(node, ConcreteType::Nil)?;
            }
            // set `node` as Class `class_name`
            IsClass(node, class_name) => {
                let node_type = ConcreteType::Class(class_name);
                let _ = self.check_or_set_type_for_node(node, node_type)?;
            }
            // When we have an exact match between 2 nodes,
            // we can derive one type from the other, but we also
            // need to check that both are actually equals
            Same(node_a, node_b) => {
                match (
                    self.solved_nodes.get(&node_a),
                    self.solved_nodes.get(&node_b),
                ) {
                    (Some(ty_a), Some(ty_b)) => {
                        if *ty_a != *ty_b {
                            return Err(TypeError::NotSame(node_a, node_b));
                        }
                    }
                    (Some(ty_a), None) => {
                        self.solved_nodes.insert(node_b, ty_a.clone());
                    }
                    (None, Some(ty_b)) => {
                        self.solved_nodes.insert(node_a, ty_b.clone());
                    }
                    (None, None) => {
                        // we haven't learn anything.
                        consumed = false;
                    }
                }
            }
            // add type to variants of `Symbols`
            SymbolCanHoldTypeOf(symbol, node) => {
                if let Some(node_ty) = self.solved_nodes.get(&node) {
                    let ty = match self.solved_symbol.remove(&symbol) {
                        Some(ConcreteType::Union(mut variants)) => {
                            if !variants.contains(node_ty) {
                                variants.push(node_ty.clone());
                            }
                            ConcreteType::Union(variants)
                        }
                        Some(sym_ty) => {
                            let mut variants = Vec::new();
                            variants.push(node_ty.clone());
                            variants.push(sym_ty);
                            ConcreteType::Union(variants)
                        }
                        None => node_ty.clone(),
                    };
                    self.solved_symbol.insert(symbol, ty);
                } else {
                    consumed = false;
                }
            }
            // assert that the type of `node` is either:
            // * an union of Num | Str,
            // * Num,
            // * or Str
            EitherNumOrStr(node) => {
                if let Some(node_ty) = self.solved_nodes.get(&node) {
                    let error = TypeError::ShouldBe(
                        node,
                        ConcreteType::Union(vec![
                            ConcreteType::Num,
                            ConcreteType::Str,
                        ]),
                    );
                    match node_ty {
                        ConcreteType::Num | ConcreteType::Str => {}
                        ConcreteType::Union(variants) => {
                            let mut required: usize = 0;
                            if variants.contains(&ConcreteType::Num) {
                                required += 1;
                            }
                            if variants.contains(&ConcreteType::Str) {
                                required += 1;
                            }
                            if required != variants.len() {
                                return Err(error);
                            }
                        }
                        _ => {
                            return Err(error);
                        }
                    }
                } else {
                    consumed = false;
                }
            }
            // propagate type of symbol into node.
            HasTypeOfSymbol(node, symbol) => {
                match (
                    self.solved_nodes.get(&node),
                    self.solved_symbol.get(&symbol),
                ) {
                    (Some(ty_node), Some(ty_symbol)) => {
                        if *ty_node != *ty_symbol {
                            return Err(TypeError::ShouldBe(
                                node,
                                ty_symbol.clone(),
                            ));
                        }
                    }
                    (None, Some(ty_symbol)) => {
                        self.solved_nodes.insert(node, ty_symbol.clone());
                    }
                    (_, None) => {
                        // we haven't learn anything.
                        consumed = false;
                    }
                }
            }
            _ => consumed = false,
        };
        Ok(consumed)
    }

    fn check_or_set_type_for_node(
        &mut self,
        node: NodeId,
        target_ty: ConcreteType,
    ) -> Result<(), TypeError> {
        if let Some(ty) = self.solved_nodes.get(&node) {
            if *ty != target_ty {
                return Err(TypeError::ShouldBe(node, target_ty));
            }
        }
        self.solved_nodes.insert(node, target_ty);
        Ok(())
    }
}

/// this struct gathers the facts derived while traversing
/// the source tree.
struct TypeSolver {
    /// facts are type constraints
    pub(crate) facts: Vec<Fact>,
    // those are traversal states:
    // current class, current super class
    in_class_declaration: Option<(SymbolId, Option<SymbolId>)>,
    solved_types: SolvedTypes,
}
impl TypeSolver {
    pub fn new() -> Self {
        Self {
            facts: Vec::new(),
            solved_types: SolvedTypes::new(),
            in_class_declaration: None,
        }
    }

    fn add_fact(&mut self, fact: Fact) -> Result<(), TypeError> {
        let was_useful = self.solved_types.use_fact(&fact)?;
        // store if for later
        if !was_useful {
            self.facts.push(fact);
        } else {
            let mut to_remove: Option<usize> = None;
            'solve_loop: loop {
                // move last fact into the one we want
                // to remove.
                // This effectively remove the facts
                // without moving the whole array, at the expense
                // of ordering, bu we don't care about that.
                if let Some(idx) = to_remove.take() {
                    if let Some(last) = self.facts.pop() {
                        self.facts[idx] = last;
                    }
                }

                for i in 0..self.facts.len() {
                    if self.solved_types.use_fact(&self.facts[i])? {
                        // schedule removal
                        to_remove = Some(i);
                        continue 'solve_loop;
                    }
                }
                break;
            }
        }

        Ok(())
    }

    /// Set the class id we are currently traversing
    /// (I don't allow class definition nesting, Bob N. does)
    pub fn set_current_class(
        &mut self,
        class: &SymbolId,
        super_class: &Option<SymbolId>,
    ) {
        self.in_class_declaration = Some((*class, *super_class));
    }

    /// Return the class ID we are currentlty traversing
    pub fn get_current_class(&self) -> Option<SymbolId> {
        self.in_class_declaration.map(|(class_id, _)| class_id)
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
    collect_expression_facts(left, solver)?;
    collect_expression_facts(right, solver)?;

    match kind {
        // both operands can only be either strings or numbers
        // _and_ are equivalent
        Add => {
            let _ = solver.add_fact(Same(binary_id, left.id));
            let _ = solver.add_fact(Same(binary_id, right.id));
            let _ = solver.add_fact(Same(left.id, right.id));
            let _ = solver.add_fact(EitherNumOrStr(left.id));
            let _ = solver.add_fact(EitherNumOrStr(right.id));
        }
        // both operands are numbers
        LessEqual | Less | GreaterEqual | Greater | Sub | Div | Mul => {
            let _ = solver.add_fact(IsNum(left.id));
            let _ = solver.add_fact(IsNum(right.id));
            let _ = solver.add_fact(IsBool(binary_id));
        }
        // both operands are equivalents
        NotEqual | Equal => {
            let _ = solver.add_fact(Same(left.id, right.id));
            let _ = solver.add_fact(IsBool(binary_id));
        }
    }
    Ok(())
}

fn collect_expression_facts(
    expr: &Expr<SymbolId>,
    solver: &mut TypeSolver,
) -> Result<(), TypeError> {
    use ExprKind::*;
    use Fact::*;
    match &expr.kind {
        // for str, number and bools,
        // we directly know the type of the expression
        // for Nil, we can't decide anything.
        Literal(literal_kind) => {
            match &literal_kind {
                LiteralKind::Num(..) => solver.add_fact(IsNum(expr.id))?,
                LiteralKind::Str(..) => solver.add_fact(IsStr(expr.id))?,
                LiteralKind::Bool(..) => solver.add_fact(IsBool(expr.id))?,
                LiteralKind::Nil => solver.add_fact(IsNil(expr.id))?,
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
                    let _ = solver.add_fact(IsBool(expr.id))?;
                }
                UnaryExprKind::Minus => {
                    // both the current expression and the innner one are numbers
                    let _ = solver.add_fact(IsNum(expr.id))?;
                    let _ = solver.add_fact(IsNum(inner_expr.id))?;
                }
            };
            collect_expression_facts(inner_expr, solver)?;
        }
        Binary(left, kind, right) => {
            collect_binary_expression_facts(expr.id, left, kind, right, solver)?
        }
        Logical(left, _kind, right) => {
            // the type of the binary expression itself depends of the evaluation,
            let _ = solver.add_fact(IsEither(expr.id, left.id, right.id))?;
            collect_expression_facts(left, solver)?;
            collect_expression_facts(right, solver)?;
        }
        Grouping(inner_expr) => {
            // the inner node has the exact same type as its child
            let _ = solver.add_fact(Same(expr.id, inner_expr.id));
            collect_expression_facts(inner_expr, solver)?;
        }
        Call(callee_expr, args) => {
            // We known that "callee_expr" is a function.
            // which should be callable with those args
            let arg_ids = args.iter().map(|arg| arg.id).collect();
            let _ = solver.add_fact(IsCallableWith(callee_expr.id, arg_ids))?;
            // we also know that the call expression
            // has the type of the return value of the expression.
            let _ = solver.add_fact(ReturnTypeOf(callee_expr.id))?;

            collect_expression_facts(callee_expr, solver)?;
            for arg_expr in args {
                collect_expression_facts(arg_expr, solver)?;
            }
        }
        Assign(bind_name, r_value_expr) => {
            // The assign expression itself is nil
            let _ = solver.add_fact(IsNil(expr.id))?;
            // the object can store the type of the `r_value_expr`
            solver
                .add_fact(SymbolCanHoldTypeOf(*bind_name, r_value_expr.id))?;
            collect_expression_facts(r_value_expr, solver)?;
        }
        Variable(bind_name) => {
            // expression has the type of the symbol
            let _ = solver.add_fact(HasTypeOfSymbol(expr.id, *bind_name))?;
        }
        Get(object_expr, attr_name) => {
            let _ = solver.add_fact(IsObjectWithAttr(
                object_expr.id,
                attr_name.clone(),
            ))?;
            collect_expression_facts(object_expr, solver)?;
            let _ = solver.add_fact(HasTypeOfObjAttr(
                expr.id,
                object_expr.id,
                attr_name.clone(),
            ))?;
        }
        Set(object_expr, attr_name, r_value_expr) => {
            let _ = solver.add_fact(IsObjectWithAttr(
                object_expr.id,
                attr_name.clone(),
            ))?;
            collect_expression_facts(object_expr, solver)?;
            collect_expression_facts(r_value_expr, solver)?;
        }
        Super(attr_name) => {
            let super_class = solver
                .get_current_super_class()
                .ok_or(TypeError::NoSuperClass)?;
            let _ = solver.add_fact(IsClass(expr.id, super_class))?;
            let _ = solver.add_fact(HasTypeOfClassAttr(
                expr.id,
                super_class,
                attr_name.clone(),
            ));
        }
        This => {
            let this_class =
                solver.get_current_class().ok_or(TypeError::NotInClass)?;
            let _ = solver.add_fact(IsClass(expr.id, this_class))?;
        }
    };
    Ok(())
}

/** Recursively collect facts in statements.
 */
fn collect_facts_in_stmt(
    stmt: &Stmt<SymbolId>,
    set: &mut TypeSolver,
) -> Result<(), TypeError> {
    use Fact::*;
    use StmtKind::*;

    match &stmt.kind {
        Block(stmts) => {
            for stmt in stmts {
                collect_facts_in_stmt(stmt, set)?;
            }
        }
        Class(name, maybe_super_name, methods) => {
            set.set_current_class(name, maybe_super_name);
            for method in methods {
                collect_facts_in_stmt(method, set)?;
            }
            set.clear_current_class();
        }
        If(condition, then, maybe_else) => todo!("If stmt"),
        Function(name, args, body) => todo!("Function"),
        Expr(expr) => {
            collect_expression_facts(expr, set)?;
        }
        Print(expr) => {
            collect_expression_facts(expr, set)?;
        }
        Return(maybe_expr) => todo!("Return"),
        Var(name, intializer) => {
            collect_expression_facts(intializer, set)?;
            let _ = set.add_fact(SymbolCanHoldTypeOf(*name, intializer.id))?;
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
    let nb_of_facts = &facts.facts.len();
    println!("remains {nb_of_facts:?} facts.");
    for fact in &facts.facts {
        println!("{fact:?}");
    }
    println!("");

    let nb_of_nodes = &facts.solved_types.solved_nodes.len();
    println!("solved {nb_of_nodes:?} nodes.");
    for node in &facts.solved_types.solved_nodes {
        println!("{node:?}");
    }
    println!("");

    let nb_of_symbols = &facts.solved_types.solved_symbol.len();
    println!("solved {nb_of_symbols:?} symbols.");
    for node in &facts.solved_types.solved_symbol {
        println!("{node:?}");
    }
    let types = TypeTable::new();
    todo!()
}
