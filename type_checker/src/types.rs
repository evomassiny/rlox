use std::ops::{Index, IndexMut};
pub enum TypeConstraint {}

pub struct Type {
    pub name: Option<String>,
    pub constraints: Vec<TypeConstraint>,
}

/// indentifier for Types,
/// (newtype pattern wrapping usize)
#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub struct TypeId(usize);

/// holds all types known by the user script
pub struct TypeTable {
    types: Vec<Type>
}

impl TypeTable {
    pub fn new() -> Self {
        Self { types: Vec::new() }
    }
}

impl Index<TypeId> for TypeTable {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index.0]
    }
}
impl IndexMut<TypeId> for TypeTable {
    fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
        &mut self.types[index.0]
    }
}

impl Index<&TypeId> for TypeTable {
    type Output = Type;

    fn index(&self, index: &TypeId) -> &Self::Output {
        &self.types[(*index).0]
    }
}

impl IndexMut<&TypeId> for TypeTable {
    fn index_mut(&mut self, index: &TypeId) -> &mut Self::Output {
        &mut self.types[(*index).0]
    }
}

