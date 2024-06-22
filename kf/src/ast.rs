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
pub struct Scope {
    pub exprs: Vec<Expression>,
}

#[derive(Debug)]
pub struct Expression {
    pub expr: ExprNode,
}

#[derive(Debug)]
pub enum ExprNode {
    Eq(Box<Expression>, Box<Expression>),
    Neq(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Ge(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Le(Box<Expression>, Box<Expression>),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Slash(Box<Expression>, Box<Expression>),
    Star(Box<Expression>, Box<Expression>),
    Bang(Box<Expression>),
    UMinus(Box<Expression>), // unary minus
    
    FnCall(String, Vec<Expression>),
    String(String),
    Literal(String),
    Char(String),
    True,
    False,
}
