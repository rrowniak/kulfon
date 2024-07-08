// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------

// In this design everything is an expression, even statements
#[derive(Debug, Clone)]
pub struct Fun {
    pub name: String,
    pub args: Vec<VarDecl>,
    pub ret: TypeDecl,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub typename: String,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub type_dec: TypeDecl,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub val: Ntype,
}

#[derive(Debug, Clone)]
pub struct Elif {
    pub cond: Box<Node>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: Box<Node>,
    pub body: Box<Node>,
    pub elif: Vec<Elif>,
    pub else_body: Option<Box<Node>>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub var_pattern: String,
    pub in_expr: Box<Node>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: Box<Node>,
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: Box<Node>,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub mutable: bool,
    pub name: String,
    pub vartype: Option<TypeDecl>,
    pub expr: Option<Box<Node>>,
}

#[derive(Debug, Clone)]
pub enum Ntype {
    // operators
    Eq(Box<Node>, Box<Node>),
    Neq(Box<Node>, Box<Node>),
    Gt(Box<Node>, Box<Node>),
    Ge(Box<Node>, Box<Node>),
    Lt(Box<Node>, Box<Node>),
    Le(Box<Node>, Box<Node>),
    Plus(Box<Node>, Box<Node>),
    Minus(Box<Node>, Box<Node>),
    Slash(Box<Node>, Box<Node>),
    Star(Box<Node>, Box<Node>),
    Bang(Box<Node>),
    UMinus(Box<Node>), // unary minus
    // assign
    Assign(String, Box<Node>),
    // control flow
    If(If),
    For(For),
    While(While),
    Loop(Loop),
    Break,
    Continue,
    // higher level structures
    Scope(Vec<Node>),
    FnDef(Fun),
    FnCall(String, Vec<Node>),
    VarDef(VarDef),
    // terminals
    String(String),
    Literal(String),
    Char(String),
    True,
    False,
}
