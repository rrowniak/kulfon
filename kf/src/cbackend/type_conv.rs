// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::type_system::EvaluatedType;

pub fn required_headers() -> Vec<&'static str> {
    vec!["stdint.h", "stddef.h"]
}

pub fn convert_built_in(kf_type: EvaluatedType) -> Option<&'static str> {
    match kf_type {
        EvaluatedType::Void => Some("void"),
        EvaluatedType::Never => Some(""),
        EvaluatedType::Bool => Some("kf_boolean"),
        EvaluatedType::U8 => Some("uint8_t"),
        EvaluatedType::U16 => Some("uint16_t"),
        EvaluatedType::U32 => Some("uint32_t"),
        EvaluatedType::U64 => Some("uint64_t"),
        EvaluatedType::U128 => None,
        EvaluatedType::I8 => Some("int8_t"),
        EvaluatedType::I16 => Some("int16_t"),
        EvaluatedType::I32 => Some("int32_t"),
        EvaluatedType::I64 => Some("int64_t"),
        EvaluatedType::I128 => None,
        EvaluatedType::F32 => Some("float"),
        EvaluatedType::F64 => Some("double"),
        EvaluatedType::USize => Some("size_t"),
        EvaluatedType::ISize => Some("ssize_t"),
        EvaluatedType::Char => Some("char"),
        EvaluatedType::Rune => Some("int32_t"),
        EvaluatedType::String => Some("str"),
        EvaluatedType::ToBeInferred => None,
        EvaluatedType::FloatingNum => None,
        EvaluatedType::Integer => None,
        EvaluatedType::Struct => None,
        EvaluatedType::Enum => None,
    }
}
