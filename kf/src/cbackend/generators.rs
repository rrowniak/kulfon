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
        EvaluatedType::String => Some("char*"),
        EvaluatedType::ToBeInferred => None,
        EvaluatedType::FloatingNum => None,
        EvaluatedType::Integer => None,
        EvaluatedType::Struct(ref name) => Some({
            let s = format!("struct {name}");
            Box::leak(s.into_boxed_str())
        }),
        EvaluatedType::Enum(ref name) => Some({
            let prefix = if ctx.tagged_enums.contains(name.as_str()) { "struct" } else { "enum" };
            let s = format!("{prefix} {name}");
            Box::leak(s.into_boxed_str())
        }),
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
        literal_s += "\\n";
    }

    for (arg_s, t, _) in args.iter().skip(1) {
        if literal_s.find("{}").is_none() {
            return format!("\"Syntax error: no placeholder {{}} for {arg_s}\"");
        }
        ret.push(match t.eval_type {
            EvaluatedType::Void | EvaluatedType::Never => {
                panic!("What the hell, trying to print void or never type?")
            }
            EvaluatedType::Bool => {
                literal_s = literal_s.replacen("{}", "%s", 1);
                format!("({arg_s})?\"true\":\"false\"")
            }
            EvaluatedType::U8
            | EvaluatedType::U16
            | EvaluatedType::U32 => {
                literal_s = literal_s.replacen("{}", "%u", 1);
                format!("{arg_s}")
            }
            EvaluatedType::U64
            | EvaluatedType::USize => {
                literal_s = literal_s.replacen("{}", "%lu", 1);
                format!("{arg_s}")
            }
            EvaluatedType::U128 => todo!(),
            EvaluatedType::I8
            | EvaluatedType::I16
            | EvaluatedType::I32 => {
                literal_s = literal_s.replacen("{}", "%d", 1);
                format!("{arg_s}")
            }
            EvaluatedType::I64
            | EvaluatedType::ISize => {
                literal_s = literal_s.replacen("{}", "%ld", 1);
                format!("{arg_s}")
            }
            EvaluatedType::I128 => todo!(),
            EvaluatedType::F32 | EvaluatedType::F64 => {
                literal_s = literal_s.replacen("{}", "%f", 1);
                format!("{arg_s}")
            }
            EvaluatedType::Char => {
                literal_s = literal_s.replacen("{}", "%c", 1);
                format!("{arg_s}")
            }
            EvaluatedType::Rune => {
                todo!()
            }
            EvaluatedType::String => {
                literal_s = literal_s.replacen("{}", "%s", 1);
                format!("{arg_s}")
            }
            EvaluatedType::ToBeInferred => panic!("ToBeInferred cannot be printed!"),
            EvaluatedType::FloatingNum | EvaluatedType::Integer => {
                panic!("Unexpected unresolved type in codegen: {t:?}")
            }
            EvaluatedType::Struct(_) => {
                literal_s = literal_s.replacen("{}", "%s", 1);
                format!("\"<struct>\"")
            }
            EvaluatedType::Enum(_) => {
                literal_s = literal_s.replacen("{}", "%s", 1);
                format!("\"<enum>\"")
            }
        });
    }
    ret.insert(0, format!("\"{literal_s}\""));
    ret.join(", ")
}

pub fn gen_struct_def(name: &str, fields: &[(String, EvaluatedType)], ctx: &mut CGenCtx) -> String {
    let mut body = String::new();
    for (fname, ftype) in fields {
        if let Some(c_type) = generate_type(ftype.clone(), ctx) {
            body += &format!("    {c_type} {fname};\n");
        }
    }
    format!("struct {name} {{\n{body}}};\n")
}

pub fn gen_enum_def(name: &str, variants: &[(String, Option<EvaluatedType>)], ctx: &mut CGenCtx) -> String {
    let has_payload = variants.iter().any(|(_, p)| p.is_some());
    if has_payload {
        gen_tagged_union_def(name, variants, ctx)
    } else {
        gen_simple_enum_def(name, variants)
    }
}

fn gen_simple_enum_def(name: &str, variants: &[(String, Option<EvaluatedType>)]) -> String {
    let variant_strs: Vec<String> = variants
        .iter()
        .map(|(vname, _)| format!("{name}_{vname}"))
        .collect();
    let variants_s = variant_strs.join(", ");
    format!("enum {name} {{ {variants_s} }};\n")
}

fn gen_tagged_union_def(name: &str, variants: &[(String, Option<EvaluatedType>)], ctx: &mut CGenCtx) -> String {
    let mut tag_variants = Vec::new();
    let mut union_fields = Vec::new();
    for (vname, payload) in variants {
        tag_variants.push(format!("    {name}_{vname}"));
        if let Some(ptype) = payload {
            if let Some(c_type) = generate_type(ptype.clone(), ctx) {
                union_fields.push(format!("        {c_type} {vname};"));
            }
        }
    }
    let tag_s = tag_variants.join(",\n");
    let union_s = union_fields.join("\n");
    format!(
        "enum {name}_tag {{\n{tag_s}\n}};\n\nstruct {name} {{\n    enum {name}_tag tag;\n    union {{\n{union_s}\n    }} payload;\n}};\n"
    )
}
