// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::cbackend::cbackend::CGenCtx;
use crate::type_system::{EvaluatedType, EvaluatedValue, KfType};

const C_STDINT_H: &str = "stdint.h";
const C_STDDEF_H: &str = "stddef.h";
const C_STDIO_H: &str = "stdio.h";

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
            ctx.stdlibs.insert(C_STDINT_H);
            Some("uint8_t")
        }
        EvaluatedType::U16 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("uint16_t")
        }
        EvaluatedType::U32 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("uint32_t")
        }
        EvaluatedType::U64 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("uint64_t")
        }
        EvaluatedType::U128 => None,
        EvaluatedType::I8 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("int8_t")
        }
        EvaluatedType::I16 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("int16_t")
        }
        EvaluatedType::I32 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("int32_t")
        }
        EvaluatedType::I64 => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
            Some("int64_t")
        }
        EvaluatedType::I128 => None,
        EvaluatedType::F32 => Some("float"),
        EvaluatedType::F64 => Some("double"),
        EvaluatedType::USize => {
            ctx.stdlibs.insert(C_STDDEF_H);
            Some("size_t")
        }
        EvaluatedType::ISize => {
            ctx.stdlibs.insert(C_STDDEF_H);
            Some("ptrdiff_t")
        }
        EvaluatedType::Char => Some("char"),
        EvaluatedType::Rune => {
            // TODO: this is C99
            ctx.stdlibs.insert(C_STDINT_H);
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

pub fn gen_fn_call(
    name: &str,
    args: &[(String, KfType, Option<EvaluatedValue>)],
    ctx: &mut CGenCtx,
) -> String {
    match name {
        "print" => {
            ctx.stdlibs.insert(C_STDIO_H);
            let args = convert_args_to_c_format(args, false);
            format!("printf({args})")
        }
        "println" => {
            ctx.stdlibs.insert(C_STDIO_H);
            let args = convert_args_to_c_format(args, true);
            format!("printf({args})")
        }
        "rand" => {
            todo!();
        }
        "assert" => {
            todo!();
        }
        "panic" => {
            todo!();
        }
        "exit" => {
            todo!();
        }
        "sleep" => {
            todo!();
        }
        "env" => {
            todo!();
        }
        "file_to_str" => {
            todo!();
        }
        "file_to_bytes" => {
            todo!();
        }
        "file_exists" => {
            todo!();
        }
        _ => {
            let args = args
                .iter()
                .map(|v| v.0.clone())
                .collect::<Vec<String>>()
                .join(", ");
            format!("{name}({args})")
        }
    }
}

fn convert_args_to_c_format(
    args: &[(String, KfType, Option<EvaluatedValue>)],
    endline: bool,
) -> String {
    // format is as follows now:
    // "this is value = {}, another value is = {}", value1, value2
    // the errors should be validated by type checker, but in case of error
    // this will be produced printf("<error description>")
    let mut ret: Vec<String> = Vec::new();
    if args.len() == 0 {
        return String::from(r#""Syntax error: zero arguments provided to the string formatter""#);
    }
    // first argument has to be a string literal
    let (_, literal_t, literal_v) = args.iter().next().unwrap();
    if literal_t.eval_type != EvaluatedType::String {
        return String::from(r#""Syntax error: expected string literal as a first argument""#);
    }

    let mut literal_s = if let Some(literal_v) = literal_v {
        match literal_v {
            EvaluatedValue::String(s) => s.clone(),
            _ => return format!("\"Syntax error: expected evaluated literal, got {literal_v:?}\""),
        }
    } else {
        return String::from(
            r#""Syntax error: expected evaluated string literal as a first argument""#,
        );
    };

    if endline {
        literal_s += "\n";
    }

    for (arg_s, t, _) in args.iter().skip(1) {
        if literal_s.find("{}").is_none() {
            return format!("\"Syntax error: no placeholder {{}} for {arg_s}\"");
        }
        ret.push(match t.eval_type {
            EvaluatedType::Bool => {
                literal_s = literal_s.replacen("{}", "%s", 1);
                format!("({arg_s})?\"true\":\"false\"")
            }
            EvaluatedType::String => {
                literal_s = literal_s.replacen("{}", "%s", 1);
                format!("\"{arg_s}\"")
            }
            _ => return String::from(r#""Syntax error: type not supported""#),
        });
    }
    ret.insert(0, format!("\"{literal_s}\""));
    ret.join(", ")
}
