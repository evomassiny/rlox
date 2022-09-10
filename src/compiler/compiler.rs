pub enum Instruction {
    Pop,
}

pub struct Compiler {
    instructions: Vec<Instruction>
}
impl Compiler {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
        }
    }
}
