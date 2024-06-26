// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------

// In this design everything is an expression, even statements
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

    GlobScope(Vec<Expression>),
    FnDef(Fun),
    FnCall(String, Vec<Expression>),
    String(String),
    Literal(String),
    Char(String),
    True,
    False,
}
