use resolver::{SymbolTable, Symbol, Ast};

pub enum TypeError { }
pub enum TypedAst { }

pub fn type_check(untyped_ast: Ast) -> Result<TypedAst, TypeError> {
    todo!()
}

