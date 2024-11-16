use super::scopes::{Globals, ScopeChain, ScopeKind};
use super::symbols::{Sym, Symbol, SymbolId, SymbolTable};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};

#[derive(Debug)]
pub enum NameError {
    /// happens when a symbol has been redefined twice in the same scope.
    RedefinitionError(String, Span, Span),
    UnboundedVariable(String, Span),
}

#[derive(Debug)]
pub struct Ast {
    pub roots: Vec<Stmt<Sym>>,
    pub symbols: SymbolTable,
}

fn resolve_block_stmt<'table>(
    stmts: Vec<Stmt<String>>,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    chain.push_scope(ScopeKind::Block);
    let mut block_stmts: Vec<Stmt<Sym>> = Vec::new();
    for in_body_stmt in stmts {
        block_stmts.push(resolve_lexical_scope(in_body_stmt, chain)?);
    }
    chain.pop_scope();
    Ok(StmtKind::Block(block_stmts))
}

fn resolve_class_stmt<'table>(
    class_name: String,
    super_name: Option<String>,
    methods: Vec<Stmt<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    let class_name_id: Sym = chain.add(class_name.clone(), src.clone());
    chain.push_scope(ScopeKind::ClassDecl);

    let super_name_id = match super_name {
        Some(super_name) => match chain.resolve(&super_name) {
            Some(super_name) => Some(super_name),
            None => return Err(NameError::UnboundedVariable(super_name, src.clone())),
        },
        None => None,
    };

    let mut out_methods: Vec<Stmt<Sym>> = Vec::new();
    for Stmt {
        kind: method_stmt_kind,
        span,
    } in methods
    {
        // validate that we're dealing with method
        let StmtKind::Function(method_name, method_args, method_body) = method_stmt_kind else {
            panic!(
                "Parsing error in {class_name}, l. {0}, class definition should only contain methods.",
                src.line,
            );
        };
        // namespace function name, to avoid collision with standard functions
        let internal_method_name = format!("{class_name}::{method_name}");
        let method_stmt_kind =
            resolve_fun_stmt(internal_method_name, method_args, method_body, &span, chain)?;
        out_methods.push(Stmt {
            kind: method_stmt_kind,
            span: span,
        });
    }
    chain.pop_scope();
    Ok(StmtKind::Class(class_name_id, super_name_id, out_methods))
}

fn resolve_fun_stmt<'table>(
    name: String,
    args: Vec<String>,
    body: Vec<Stmt<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    // add function name binding to parent scope
    let name: Sym = chain.add(name, src.clone());

    chain.push_scope(ScopeKind::FunDecl);

    // add argument bindings to function scope
    let mut arg_ids = Vec::new();
    for arg_name in args {
        arg_ids.push(chain.add(arg_name, src.clone()));
    }

    // resolve body statements
    let mut resolved_stmts = Vec::new();
    for stmt in body {
        resolved_stmts.push(resolve_lexical_scope(stmt, chain)?);
    }

    chain.pop_scope();
    Ok(StmtKind::Function(name, arg_ids, resolved_stmts))
}

/// resolve the condition expression,
/// and the two branches (then/else) independantly
fn resolve_if_stmt<'table>(
    condition: Box<Expr<String>>,
    then_branch: Box<Stmt<String>>,
    maybe_else_branch: Option<Box<Stmt<String>>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    // validate the condition expression
    let condition: Box<Expr<Sym>> = resolve_expression(condition, src, chain)?;
    // validate the "then" branch,
    // we need to introduce a new scope, otherwise a variable defined in the
    // "then" branch could be used in the "else" one.
    chain.push_scope(ScopeKind::Block);
    let then_branch: Box<Stmt<Sym>> = Box::new(resolve_lexical_scope(*then_branch, chain)?);
    chain.pop_scope();
    // validate "else" branch
    let maybe_else_branch: Option<Box<Stmt<Sym>>> = match maybe_else_branch {
        Some(else_branch) => {
            chain.push_scope(ScopeKind::Block);
            let else_branch = Box::new(resolve_lexical_scope(*else_branch, chain)?);
            chain.pop_scope();
            Some(else_branch)
        }
        None => None,
    };
    Ok(StmtKind::If(condition, then_branch, maybe_else_branch))
}

/// resolve the bindings in a While statement
fn resolve_while_stmt<'table>(
    condition: Box<Expr<String>>,
    body: Box<Stmt<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    // validate the condition expression
    let condition: Box<Expr<Sym>> = resolve_expression(condition, src, chain)?;
    // validate body statement
    chain.push_scope(ScopeKind::Block);
    let body: Box<Stmt<Sym>> = Box::new(resolve_lexical_scope(*body, chain)?);
    chain.pop_scope();
    Ok(StmtKind::While(condition, body))
}
/// resolve the bindings in a For statement
fn resolve_for_stmt<'table>(
    maybe_initializer: Option<Box<Expr<String>>>,
    maybe_condition: Option<Box<Expr<String>>>,
    maybe_increment: Option<Box<Expr<String>>>,
    body: Box<Stmt<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    // validate "for(initializer, condition, increment)"
    let maybe_initializer: Option<Box<Expr<Sym>>> = match maybe_initializer {
        Some(initializer) => Some(resolve_expression(initializer, src, chain)?),
        None => None,
    };
    let maybe_condition: Option<Box<Expr<Sym>>> = match maybe_condition {
        Some(condition) => Some(resolve_expression(condition, src, chain)?),
        None => None,
    };
    let maybe_increment: Option<Box<Expr<Sym>>> = match maybe_increment {
        Some(increment) => Some(resolve_expression(increment, src, chain)?),
        None => None,
    };

    // validate for body
    chain.push_scope(ScopeKind::Block);
    let body: Box<Stmt<Sym>> = Box::new(resolve_lexical_scope(*body, chain)?);
    chain.pop_scope();
    Ok(StmtKind::For(
        maybe_initializer,
        maybe_condition,
        maybe_increment,
        body,
    ))
}
/// register the new variable binding,
/// and validate the initializer expression
fn resolve_var_stmt<'table>(
    name: String,
    intializer: Box<Expr<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    // check that the variable was not already defined in the same scope
    if let Some(previous_decl) = chain.location_of_declaration_in_current_scope(&name) {
        return Err(NameError::RedefinitionError(
            name,
            src.clone(),
            previous_decl,
        ));
    }
    // validate intializer expression
    let initializer_expr: Box<Expr<Sym>> = resolve_expression(intializer, src, chain)?;

    // register binding
    let name: Sym = chain.add(name, src.clone());

    Ok(StmtKind::Var(name, initializer_expr))
}

/// resolve the expression being printed
fn resolve_print_stmt<'table>(
    in_expr: Box<Expr<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    let out_expr: Box<Expr<Sym>> = resolve_expression(in_expr, src, chain)?;
    Ok(StmtKind::Print(out_expr))
}

/// resolve the expression being returned
fn resolve_return_stmt<'table>(
    maybe_expr: Option<Box<Expr<String>>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    let maybe_expression: Option<Box<Expr<Sym>>> = match maybe_expr {
        Some(expr) => Some(resolve_expression(expr, src, chain)?),
        None => None,
    };
    Ok(StmtKind::Return(maybe_expression))
}

/// validate expression names
/// in an expression statement.
fn resolve_expr_stmt<'table>(
    in_expr: Box<Expr<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<StmtKind<Sym>, NameError> {
    let out_expr: Box<Expr<Sym>> = resolve_expression(in_expr, src, chain)?;
    Ok(StmtKind::Expr(out_expr))
}

/// validate expression names
fn resolve_expression<'table>(
    in_expr: Box<Expr<String>>,
    src: &Span,
    chain: &mut ScopeChain<'table>,
) -> Result<Box<Expr<Sym>>, NameError> {
    use ExprKind::*;
    // TODO:
    // use an explicit stack for recursion instead of the stackframe
    let out_kind: ExprKind<Sym> = match in_expr.kind {
        Literal(literal_kind) => Literal(literal_kind),
        Unary(kind, inner_expr) => {
            let inner_out_expr = resolve_expression(inner_expr, src, chain)?;
            ExprKind::Unary(kind, inner_out_expr)
        }
        Binary(left_expr, kind, right_expr) => {
            let left_out_expr = resolve_expression(left_expr, src, chain)?;
            let right_out_expr = resolve_expression(right_expr, src, chain)?;
            ExprKind::Binary(left_out_expr, kind, right_out_expr)
        }
        Logical(left_expr, kind, right_expr) => {
            let left_out_expr = resolve_expression(left_expr, src, chain)?;
            let right_out_expr = resolve_expression(right_expr, src, chain)?;
            ExprKind::Logical(left_out_expr, kind, right_out_expr)
        }
        Grouping(inner_expr) => {
            let inner_out_expr = resolve_expression(inner_expr, src, chain)?;
            ExprKind::Grouping(inner_out_expr)
        }
        Call(callee_expr, args) => {
            let callee = resolve_expression(callee_expr, src, chain)?;
            let mut out_args = Vec::with_capacity(args.len());
            for in_arg in args {
                out_args.push(*resolve_expression(Box::new(in_arg), src, chain)?);
            }
            ExprKind::Call(callee, out_args)
        }
        Assign(bind_name, r_value_expr) => {
            let symbol_id = match chain.resolve(&bind_name) {
                Some(symbol_id) => symbol_id,
                None => return Err(NameError::UnboundedVariable(bind_name, src.clone())),
            };
            let r_value_expr = resolve_expression(r_value_expr, src, chain)?;
            ExprKind::Assign(symbol_id, r_value_expr)
        }
        Variable(bind_name) => {
            let symbol_id = match chain.resolve(&bind_name) {
                Some(symbol_id) => symbol_id,
                None => return Err(NameError::UnboundedVariable(bind_name, src.clone())),
            };
            ExprKind::Variable(symbol_id)
        }
        Get(object_expr, attr_name) => {
            let object_expr = resolve_expression(object_expr, src, chain)?;
            ExprKind::Get(object_expr, attr_name)
        }
        Set(object_expr, attr_name, r_value_expr) => {
            let object_expr = resolve_expression(object_expr, src, chain)?;
            let r_value_expr = resolve_expression(r_value_expr, src, chain)?;
            ExprKind::Set(object_expr, attr_name, r_value_expr)
        }
        // no-op
        Super(attr_name) => Super(attr_name),
        // no-op
        This => This,
    };
    let out_expr = Expr {
        kind: out_kind,
        span: in_expr.span,
    };
    Ok(Box::new(out_expr))
}

/// recursively traverse the AST starting from `in_stmt`,
/// and resolve variable names along the way.
fn resolve_lexical_scope<'table>(
    in_stmt: Stmt<String>,
    chain: &mut ScopeChain<'table>,
) -> Result<Stmt<Sym>, NameError> {
    use StmtKind::*;
    let out_kind: StmtKind<Sym> = match in_stmt.kind {
        Block(stmts) => resolve_block_stmt(stmts, chain)?,
        Class(name, maybe_super_name, methods) => {
            resolve_class_stmt(name, maybe_super_name, methods, &in_stmt.span, chain)?
        }
        If(condition, then, maybe_else) => {
            resolve_if_stmt(condition, then, maybe_else, &in_stmt.span, chain)?
        }
        Function(name, args, body) => resolve_fun_stmt(name, args, body, &in_stmt.span, chain)?,
        Expr(expr) => resolve_expr_stmt(expr, &in_stmt.span, chain)?,
        Print(expr) => resolve_print_stmt(expr, &in_stmt.span, chain)?,
        Return(maybe_expr) => resolve_return_stmt(maybe_expr, &in_stmt.span, chain)?,
        Var(name, intializer) => resolve_var_stmt(name, intializer, &in_stmt.span, chain)?,
        While(condition, body) => resolve_while_stmt(condition, body, &in_stmt.span, chain)?,
        For(maybe_initializer, maybe_condition, maybe_increment, body) => resolve_for_stmt(
            maybe_initializer,
            maybe_condition,
            maybe_increment,
            body,
            &in_stmt.span,
            chain,
        )?,
    };
    Ok(Stmt {
        kind: out_kind,
        span: in_stmt.span,
    })
}

/// Resolve all globals from the top ast node
fn resolve_globals(in_ast: &Vec<Stmt<String>>, symbols: &mut SymbolTable) -> Globals {
    let mut globals = Globals::new();
    use StmtKind::*;
    for stmt in in_ast {
        match stmt.kind {
            Class(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), symbols);
            }
            Function(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), symbols);
            }
            Var(ref name, ..) => {
                globals.add(name.clone(), stmt.span.clone(), symbols);
            }
            _ => continue,
        }
    }
    globals
}

/// traverse the input Abstract Syntax Tree,
/// and do name resolution.
///
/// In the process, create a symbol table, and use references to its content
/// in the output AST
pub fn resolve_names(in_ast: Vec<Stmt<String>>) -> Result<Ast, NameError> {
    // Traverse the AST in 2 passes:
    // * do a quick shallow pass, to locate globals
    // * do a real traversal, resolving everything

    let mut symbols = SymbolTable::new();

    // 1. resolve globals
    let globals = resolve_globals(&in_ast, &mut symbols);

    // 2. perform a pre-order tree traversal,
    // and maintains some kind of lexical scope chain,
    // during the traversal, we update `symbols`
    let mut chain = ScopeChain::new(globals, &mut symbols);
    let mut out_stmts: Vec<Stmt<Sym>> = Vec::new();
    for stmt in in_ast {
        let stmt = resolve_lexical_scope(stmt, &mut chain)?;
        out_stmts.push(stmt);
    }

    // 3. gather outputs
    Ok(Ast {
        roots: out_stmts,
        symbols,
    })
}
