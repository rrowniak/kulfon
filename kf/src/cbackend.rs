// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::ast;

type ResultC = Result<String, String>;
const INDENT_LEVEL: usize = 4;

pub fn gen_c_code(ast: &ast::Expression) -> ResultC {
    process_expr(ast, 0)
}

fn process_expr(expr: &ast::Expression, indent: usize) -> ResultC {
    match &expr.expr {
        ast::ExprNode::GlobScope(glob) => {
            let mut code = String::new();
            for expr in glob {
                code += "\n";
                code += &process_expr(expr, indent)?;
            }
            Ok(code)
        }
        ast::ExprNode::FnDef(fn_def) => transform_fn(&fn_def, indent),
        ast::ExprNode::FnCall(fn_name, fn_args) => transform_fn_call(fn_name, fn_args, indent),
        ast::ExprNode::Eq(a, b) => Ok(format!(
            "{}{} == {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Neq(a, b) => Ok(format!(
            "{}{} != {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Gt(a, b) => Ok(format!(
            "{}{} > {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Ge(a, b) => Ok(format!(
            "{}{} >= {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Lt(a, b) => Ok(format!(
            "{}{} < {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Le(a, b) => Ok(format!(
            "{}{} <= {}",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Plus(a, b) => Ok(format!(
            "{}({} + {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Minus(a, b) => Ok(format!(
            "{}({} - {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Slash(a, b) => Ok(format!(
            "{}({} / {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Star(a, b) => Ok(format!(
            "{}({} * {})",
            indent_str(indent),
            process_expr(a, 0)?,
            process_expr(b, 0)?
        )),
        ast::ExprNode::Bang(a) => Ok(format!("{}!{}", indent_str(indent), process_expr(a, 0)?,)),
        ast::ExprNode::UMinus(a) => Ok(format!("{}-{}", indent_str(indent), process_expr(a, 0)?,)),

        ast::ExprNode::String(s) => Ok(format!("{}\"{}\"", indent_str(indent), s)),
        ast::ExprNode::Literal(s) => Ok(format!("{}{}", indent_str(indent), s)),
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
    let mut body = String::new();
    for e in fn_def.body.exprs.iter() {
        body += "\n";
        body += &process_expr(&e, indent + INDENT_LEVEL)?;
    }
    let c_fn = format!(
        "{indent_s}{ret_type} {} ({args}) {{{body} \n{indent_s}}}",
        fn_def.name,
    );
    Ok(c_fn)
}

fn transform_fn_call(name: &str, args: &Vec<ast::Expression>, indent: usize) -> ResultC {
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
