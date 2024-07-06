// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::type_system::BuiltInT;

pub fn required_headers() -> Vec<&'static str> {
    vec!["<stdint.h>", "<stddef.h>"]
}

pub fn convert_built_in(kf_type: BuiltInT) -> Option<&'static str> {
    match kf_type {
        BuiltInT::Bool => Some("kf_boolean"),
        BuiltInT::U8 => Some("uint8_t"),
        BuiltInT::U16 => Some("uint16_t"),
        BuiltInT::U32 => Some("uint32_t"),
        BuiltInT::U64 => Some("uint64_t"),
        BuiltInT::U128 => None,
        BuiltInT::I8 => Some("int8_t"),
        BuiltInT::I16 => Some("int16_t"),
        BuiltInT::I32 => Some("int32_t"),
        BuiltInT::I64 => Some("int64_t"),
        BuiltInT::I128 => None,
        BuiltInT::F32 => Some("float"),
        BuiltInT::F64 => Some("double"),
        BuiltInT::USize => Some("size_t"),
        BuiltInT::ISize => Some("ssize_t"),
        BuiltInT::Char => Some("char"),
        BuiltInT::Rune => Some("int32_t"),
        BuiltInT::Inferred => None,
    }
}
