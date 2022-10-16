#[derive(Debug)]
pub enum ObjFnKind {
    Script,
    Function,
    Initializer,
    Method,
}
#[derive(Debug)]
pub struct ObjFunction {}
#[derive(Debug)]
pub struct Local {}
#[derive(Debug)]
pub struct UpValue {}

#[derive(Debug)]
pub struct CompileUnit {
    pub function: ObjFunction,
    kind: ObjFnKind,
    locals: Vec<Local>,
    upvalues: Vec<UpValue>,
    scope_depth: usize,
}

impl CompileUnit {
    pub fn new_script() -> Self {
        Self {
            function: ObjFunction {},
            kind: ObjFnKind::Script,
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
        }
    }
}
