use resolver::{Ast, Symbol, SymbolId, SymbolTable};
use lexer::Span;
use parser::{Expr, ExprKind, Stmt, StmtKind};
use super::{Type, TypeId, TypeTable, TypeConstraint};

#[derive(Debug)]
pub enum TypeError { }

#[derive(Debug)]
pub struct TypedAst { }

pub struct SymbolTypes {
    type_id_by_symbol_id: Vec<Option<TypeId>>,
}


pub fn type_check(untyped_ast: Ast) -> Result<TypedAst, TypeError> {
    let mut types = TypeTable::new();
    todo!()
}

