// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedType {
    // built-in types
    Void,
    Never,
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    USize,
    ISize,
    Char,
    Rune,
    String,
    // to be deduced by compiler
    ToBeInferred,
    // partially inferred from literal
    FloatingNum,
    Integer,
    // Complex types
    Struct,
    Enum,
}

impl EvaluatedType {
    ///
    /// Converts explicitly defined built-in type
    ///
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "void" => Some(Self::Void),
            "bool" => Some(Self::Bool),
            "u8" => Some(Self::U8),
            "u16" => Some(Self::U16),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "u128" => Some(Self::U128),
            "i8" => Some(Self::I8),
            "i16" => Some(Self::I16),
            "i32" => Some(Self::I32),
            "i64" => Some(Self::I64),
            "i128" => Some(Self::I128),
            "f32" => Some(Self::F32),
            "f64" => Some(Self::F64),
            "usize" => Some(Self::USize),
            "isize" => Some(Self::ISize),
            "char" => Some(Self::Char),
            "rune" => Some(Self::Rune),
            "str" => Some(Self::String),
            "_" => Some(Self::ToBeInferred),
            _ => None,
        }
    }
    ///
    /// Checks if given literal represents a built-in type
    ///
    pub fn is_builtin(s: &str) -> bool {
        Self::from_str(s).is_some()
    }
    pub fn is_numeric(&self) -> bool {
        *self == Self::I8
            || *self == Self::I16
            || *self == Self::I32
            || *self == Self::I64
            || *self == Self::I128
            || *self == Self::U8
            || *self == Self::U16
            || *self == Self::U32
            || *self == Self::U64
            || *self == Self::U128
            || *self == Self::ISize
            || *self == Self::USize
            || *self == Self::Integer
    }
    pub fn is_floating(&self) -> bool {
        *self == Self::F32 || *self == Self::F64 || *self == Self::FloatingNum
    }
}

#[derive(Debug)]
pub struct KfType {
    pub mutable: Option<bool>,
    pub eval_type: EvaluatedType,
}

impl KfType {
    pub fn from_literal(t: EvaluatedType) -> Self {
        Self {
            mutable: Some(false),
            eval_type: t,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Int {
    Signed(i128),
    Unsigned(u128),
}

#[derive(Debug, PartialEq, Clone)]
pub struct EvaluatedInt {
    pub signed: bool,
    pub min_bits: usize,
    pub val: Int,
}

#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedFloat {
    F32(f32),
    F64(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub enum EvaluatedValue {
    Bool(bool),
    Integer(EvaluatedInt),
    Floating(EvaluatedFloat),
    Char(char),
    Rune(i32),
    String(String),
}

impl EvaluatedValue {
    pub fn try_to_i128(&self) -> Option<i128> {
        match self {
            Self::Integer(ev_int) => match ev_int.val {
                Int::Signed(i) => Some(i),
                Int::Unsigned(u) => Some(u as i128),
            },
            Self::Char(c) => Some(*c as i128),
            Self::Rune(r) => Some(*r as i128),
            Self::Bool(_) | Self::Floating(_) | Self::String(_) => None,
        }
    }
}
