mod type_check;
mod types;

pub use types::{Type, TypeId, TypeConstraint, TypeTable };
pub use type_check::{TypeError, TypedAst, type_check };
