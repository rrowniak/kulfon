// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::ast;

type ResultC = Result<String, String>;
const INDENT_LEVEL: usize = 4;

pub fn gen_c_code(ast: &ast::Node) -> ResultC {
    process_expr(ast, 0)
}

fn process_expr(expr: &ast::Node, indent: usize) -> ResultC {
    match &expr.val {
        ast::Ntype::Scope(glob) => {
            let mut code = String::new();
            for expr in glob {
                code += "\n";
                code += &process_expr(expr, indent)?;
            }
            Ok(code)
        }
        ast::Ntype::FnDef(fn_def) => transform_fn(&fn_def, indent),
        ast::Ntype::FnCall(fn_name, fn_args) => transform_fn_call(fn_name, fn_args, indent),
        ast::Ntype::Eq(a, b) => Ok(format!(
            "{}{} == {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Neq(a, b) => Ok(format!(
            "{}{} != {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Gt(a, b) => Ok(format!(
            "{}{} > {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Ge(a, b) => Ok(format!(
            "{}{} >= {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Lt(a, b) => Ok(format!(
            "{}{} < {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Le(a, b) => Ok(format!(
            "{}{} <= {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Plus(a, b) => Ok(format!(
            "{}({} + {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Minus(a, b) => Ok(format!(
            "{}({} - {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Slash(a, b) => Ok(format!(
            "{}({} / {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Star(a, b) => Ok(format!(
            "{}({} * {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::Ntype::Bang(a) => Ok(format!("{}!{}", indent_str(indent), process_expr(a, 0)?,)),
        ast::Ntype::UMinus(a) => Ok(format!("{}-{}", indent_str(indent), process_expr(a, 0)?,)),

        ast::Ntype::String(s) => Ok(format!("{}\"{}\"", indent_str(indent), s)),
        ast::Ntype::Literal(s) => Ok(format!("{}{}", indent_str(indent), s)),
        other => Err(format!("C backend: unsupported expression: {:?}", other)),
    }
}

fn transform_fn(fn_def: &ast::Fun, indent: usize) -> ResultC {
    let indent_s = indent_str(indent);
    let ret_type = if fn_def.ret.typename.len() == 0 {
        "void".into()
    } else {
        fn_def.ret.typename.clone()
    };
    let args = String::new();
    let body = transform_scope(&fn_def.body, indent + INDENT_LEVEL)?;
    let c_fn = format!(
        "{indent_s}{ret_type} {} ({args}) {{{body} \n{indent_s}}}",
        fn_def.name,
    );
    Ok(c_fn)
}

fn transform_scope(scope: &ast::Node, indent: usize) -> ResultC {
    let scope_nodes = match &scope.val {
        ast::Ntype::Scope(v) => v,
        _ => return Err("Expected Scope ast::Node, got something else".into()),
    };
    let mut scope_str = String::new();
    for e in scope_nodes.iter() {
        scope_str += "\n";
        scope_str += &process_expr(&e, indent)?;
    }
    Ok(scope_str)
}

fn transform_fn_call(name: &str, args: &Vec<ast::Node>, indent: usize) -> ResultC {
    let indent_s = indent_str(indent);
    let mut args_s = String::new();
    for (i, e) in args.iter().enumerate() {
        args_s += &process_expr(&e, 0)?;
        if i < args.len() - 1 {
            args_s += ", ";
        }
    }
    let statement = format!("{indent_s}{name}({args_s});");
    Ok(statement)
}

fn indent_str(indent: usize) -> String {
    " ".repeat(indent)
}
