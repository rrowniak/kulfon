// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum BuiltInT {
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
    Inferred,
}

impl BuiltInT {
    pub fn from_str(s: &str) -> Option<BuiltInT> {
        match s {
            "bool" => Some(BuiltInT::Bool),
            "u8" => Some(BuiltInT::U8),
            "u16" => Some(BuiltInT::U16),
            "u32" => Some(BuiltInT::U32),
            "u64" => Some(BuiltInT::U64),
            "u128" => Some(BuiltInT::U128),
            "i8" => Some(BuiltInT::I8),
            "i16" => Some(BuiltInT::I16),
            "i32" => Some(BuiltInT::I32),
            "i64" => Some(BuiltInT::I64),
            "i128" => Some(BuiltInT::I128),
            "f32" => Some(BuiltInT::F32),
            "f64" => Some(BuiltInT::F64),
            "usize" => Some(BuiltInT::USize),
            "isize" => Some(BuiltInT::ISize),
            "char" => Some(BuiltInT::Char),
            "rune" => Some(BuiltInT::Rune),
            "_" => Some(BuiltInT::Inferred),
            _ => None,
        }
    }

    pub fn is_builtin(s: &str) -> bool {
        Self::from_str(s).is_some()
    }
}
