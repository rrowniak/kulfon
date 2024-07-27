// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------
use crate::comp_msg::TextPoint;

#[derive(Debug, Clone)]
pub struct Fun {
    pub name: String,
    pub args: Vec<VarDecl>,
    pub ret: TypeDecl,
    pub body: Box<Node>,
}
#[derive(Debug, Clone)]
pub enum RefType {
    Borrow,
    BorrowMut,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Array(Box<TypeDecl>, Option<Box<Node>>),
    Slice(Box<TypeDecl>),
    JustName(String),
    JustNameGeneric(String, Vec<TypeDecl>),
}

#[derive(Debug, Clone)]
pub struct TypeDecl {
    pub reference_stack: Vec<RefType>,
    pub scope_res_relative: bool,
    pub scope_resolution: Vec<String>,
    pub type_id: TypeKind,
}

impl TypeDecl {
    pub fn new(typename: String) -> Self {
        Self {
            reference_stack: Vec::new(),
            scope_res_relative: true,
            scope_resolution: Vec::new(),
            type_id: TypeKind::JustName(typename),
        }
    }

    pub fn typename(&self) -> Option<String> {
        match &self.type_id {
            TypeKind::JustName(n) => Some(n.clone()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub name: String,
    pub type_dec: TypeDecl,
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
pub struct Struct {
    pub name: String,
    pub members: Vec<VarDecl>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub enums: Vec<(String, Option<TypeDecl>)>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub name: String,
    pub scope: Vec<Node>,
}

#[derive(Debug, Clone)]
pub struct Node {
    pub val: Ntype,
    pub at: TextPoint,
    pub meta_idx: Option<usize>,
}

impl Node {
    pub fn new(val: Ntype, at: TextPoint) -> Self {
        Node {
            val,
            at,
            meta_idx: None,
        }
    }
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
    Struct(Struct),
    Enum(Enum),
    Impl(Impl),
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
