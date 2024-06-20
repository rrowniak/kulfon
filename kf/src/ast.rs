#[derive(Debug)]
pub struct GlobScope {
    pub imports: Vec<Import>,
    pub fns: Vec<Fun>,
}

#[derive(Debug)]
pub struct Import {}

#[derive(Debug)]
pub struct Fun {
    pub name: String,
    pub args: Vec<VarDecl>,
    pub ret: TypeDecl,
    pub body: Scope,
}

#[derive(Debug)]
pub struct TypeDecl {
    pub typename: String,
}

#[derive(Debug)]
pub struct VarDecl {
    pub name: String,
    pub type_dec: TypeDecl,
}

#[derive(Debug)]
pub struct Scope {}
