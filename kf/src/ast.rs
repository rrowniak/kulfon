// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------
use crate::comp_msg::TextPoint;

// tree structure will be represented as a flast AST
// https://www.cs.cornell.edu/~asampson/blog/flattening.html

#[derive(Copy, Clone, Debug)]
pub struct NodeRef(i32);

impl NodeRef {
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

pub type NodeRefs = Vec<NodeRef>;

#[derive(Debug, Clone)]
pub struct Fun {
    pub name: String,
    pub args: Vec<VarDecl>,
    pub variadic: bool,
    pub ret: TypeDecl,
    pub body: NodeRef,
}
#[derive(Debug, Clone)]
pub enum RefType {
    Borrow,
    BorrowMut,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Array(Box<TypeDecl>, Option<NodeRef>),
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
    pub cond: NodeRef,
    pub body: NodeRef,
}

#[derive(Debug, Clone)]
pub struct If {
    pub cond: NodeRef,
    pub body: NodeRef,
    pub elif: Vec<Elif>,
    pub else_body: Option<NodeRef>,
}

#[derive(Debug, Clone)]
pub struct For {
    pub var_pattern: String,
    pub in_expr: NodeRef,
    pub body: NodeRef,
}

#[derive(Debug, Clone)]
pub struct While {
    pub cond: NodeRef,
    pub body: NodeRef,
}

#[derive(Debug, Clone)]
pub struct Loop {
    pub body: NodeRef,
}

#[derive(Debug, Clone)]
pub struct VarDef {
    pub mutable: bool,
    pub name: String,
    pub vartype: Option<TypeDecl>,
    pub expr: Option<NodeRef>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub generic_args: Vec<TypeDecl>,
    pub members: Vec<VarDecl>,
}

#[derive(Debug, Clone)]
pub struct Enum {
    pub name: String,
    pub generic_args: Vec<TypeDecl>,
    pub enums: Vec<(String, Option<TypeDecl>)>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub name: String,
    pub scope: NodeRefs,
}

#[derive(Debug, Clone)]
pub struct Tree {
    pub root: NodeRef,
    pub flat: Vec<Node>,
}

impl Tree {
    pub fn new(cap: usize) -> Self {
        Self {
            root: NodeRef(0), // kinda dodgy as this is no valid Id so far
            flat: Vec::with_capacity(cap),
        }
    }
    pub fn push(&mut self, node: Node) -> NodeRef {
        let nref = NodeRef(self.flat.len() as i32);
        self.flat.push(node);
        nref
    }

    pub fn get(&self, nref: NodeRef) -> &Node {
        &self.flat[nref.0 as usize]
    }
}

#[derive(Debug, Clone)]
pub struct Node {
    pub val: Ntype,
    pub at: TextPoint,
    // pub meta_idx: Option<usize>,
}

impl Node {
    pub fn new(val: Ntype, at: TextPoint) -> Self {
        Node {
            val,
            at,
            // meta_idx: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Ntype {
    // operators
    Eq(NodeRef, NodeRef),
    Neq(NodeRef, NodeRef),
    Gt(NodeRef, NodeRef),
    Ge(NodeRef, NodeRef),
    Lt(NodeRef, NodeRef),
    Le(NodeRef, NodeRef),
    Plus(NodeRef, NodeRef),
    Minus(NodeRef, NodeRef),
    Slash(NodeRef, NodeRef),
    Star(NodeRef, NodeRef),
    Bang(NodeRef),
    UMinus(NodeRef), // unary minus
    // assign
    Assign(String, NodeRef),
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
    Scope(NodeRefs),
    FnDef(Fun),
    FnCall(String, NodeRefs),
    VarDef(VarDef),
    // terminals
    String(String),
    Literal(String),
    Char(String),
    True,
    False,
}
