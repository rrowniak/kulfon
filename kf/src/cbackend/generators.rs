// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::cbackend::cbackend::CGenCtx;
use crate::type_system::EvaluatedType;

pub fn gen_bool_def() -> &'static str {
    "typedef enum {false, true} kf_boolean;"
}

pub fn gen_bool_var(var: bool, ctx: &mut CGenCtx) -> &'static str {
    ctx.bool_in_use = true;
    if var {
        "true"
    } else {
        "false"
    }
}

pub fn generate_type(kf_type: EvaluatedType, ctx: &mut CGenCtx) -> Option<&'static str> {
    match kf_type {
        EvaluatedType::Void => Some("void"),
        EvaluatedType::Never => Some(""),
        EvaluatedType::Bool => {
            ctx.bool_in_use = true;
            Some("kf_boolean")
        }
        EvaluatedType::U8 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("uint8_t")
        }
        EvaluatedType::U16 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("uint16_t")
        }
        EvaluatedType::U32 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("uint32_t")
        }
        EvaluatedType::U64 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("uint64_t")
        }
        EvaluatedType::U128 => None,
        EvaluatedType::I8 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("int8_t")
        }
        EvaluatedType::I16 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("int16_t")
        }
        EvaluatedType::I32 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("int32_t")
        }
        EvaluatedType::I64 => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("int64_t")
        }
        EvaluatedType::I128 => None,
        EvaluatedType::F32 => Some("float"),
        EvaluatedType::F64 => Some("double"),
        EvaluatedType::USize => {
            ctx.stdlibs.insert("stddef.h");
            Some("size_t")
        }
        EvaluatedType::ISize => {
            ctx.stdlibs.insert("stddef.h");
            Some("ptrdiff_t")
        }
        EvaluatedType::Char => Some("char"),
        EvaluatedType::Rune => {
            // TODO: this is C99
            ctx.stdlibs.insert("stdint.h");
            Some("int32_t")
        }
        EvaluatedType::String => Some("str"),
        EvaluatedType::ToBeInferred => None,
        EvaluatedType::FloatingNum => None,
        EvaluatedType::Integer => None,
        EvaluatedType::Struct => None,
        EvaluatedType::Enum => None,
    }
}
