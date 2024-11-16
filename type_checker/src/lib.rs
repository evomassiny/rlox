mod type_check;
mod types;

pub use type_check::{type_check, TypeError, TypedAst};
pub use types::{Type, TypeConstraint, TypeId, TypeTable};
