// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::ast;
use crate::kf_core;
use std::collections::{HashMap, HashSet};

pub type ResultC = Result<String, String>;
const INDENT_LEVEL: usize = 4;

pub fn gen_c_code(mid: &kf_core::InterRepr) -> ResultC {
    CGen::new(mid).gen()
}

struct CGen<'a> {
    mid: &'a kf_core::InterRepr,
    scope_depth: usize,
    cstd_calls: HashSet<String>,
    fun_to_header_map: HashMap<&'static str, &'static str>,
    bool_in_use: bool,
}

impl<'a> CGen<'a> {
    fn new(mid: &'a kf_core::InterRepr) -> Self {
        Self {
            mid,
            scope_depth: 0,
            cstd_calls: HashSet::new(),
            fun_to_header_map: HashMap::from([("printf", "stdio.h")]),
            bool_in_use: false,
        }
    }

    fn gen(&mut self) -> ResultC {
        // println!("{:?}", self.mid.syntree);
        let code = self.process_ast_node(&self.mid.syntree, 0)?;
        let mut ret = self.gen_include_directives()?;
        ret += "\n";
        if self.bool_in_use {
            ret += self.gen_bool_def();
            ret += "\n";
        }
        ret += code.as_str();
        Ok(ret)
    }

    fn process_ast_node(&mut self, expr: &ast::Node, indent: usize) -> ResultC {
        match &expr.val {
            ast::Ntype::Scope(scope) => {
                self.scope_depth += 1;
                let r = self.transform_scope(&scope, indent);
                self.scope_depth -= 1;
                r
            }
            ast::Ntype::FnDef(fn_def) => self.transform_fn(&fn_def, indent),
            ast::Ntype::FnCall(fn_name, fn_args) => {
                self.transform_fn_call(fn_name, fn_args, indent)
            }
            ast::Ntype::Eq(a, b) => Ok(format!(
                "{}{} == {}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Neq(a, b) => Ok(format!(
                "{}{} != {}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Gt(a, b) => Ok(format!(
                "{}{} > {}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Ge(a, b) => Ok(format!(
                "{}{} >= {}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Lt(a, b) => Ok(format!(
                "{}{} < {}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Le(a, b) => Ok(format!(
                "{}{} <= {}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Plus(a, b) => Ok(format!(
                "{}({} + {})",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Minus(a, b) => Ok(format!(
                "{}({} - {})",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Slash(a, b) => Ok(format!(
                "{}({} / {})",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Star(a, b) => Ok(format!(
                "{}({} * {})",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
                self.process_ast_node(b, 0)?
            )),
            ast::Ntype::Bang(a) => Ok(format!(
                "{}!{}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
            )),
            ast::Ntype::UMinus(a) => Ok(format!(
                "{}-{}",
                self.indent_str(indent),
                self.process_ast_node(a, 0)?,
            )),
            ast::Ntype::False => {
                self.bool_in_use = true;
                Ok(format!("false"))
            }
            ast::Ntype::True => {
                self.bool_in_use = true;
                Ok(format!("true"))
            }
            ast::Ntype::If(if_) => self.transform_if(if_, indent),
            ast::Ntype::For(for_) => todo!(),
            ast::Ntype::While(while_) => todo!(),
            ast::Ntype::Loop(loop_) => todo!(),
            ast::Ntype::VarDef(var) => todo!(),
            ast::Ntype::String(s) => Ok(format!("{}\"{}\"", self.indent_str(indent), s)),
            ast::Ntype::Char(s) => Ok(format!("'{s}'")),
            ast::Ntype::Literal(s) => Ok(format!("{}{}", self.indent_str(indent), s)),
            // other => Err(format!("C backend: unsupported expression: {:?}", other)),
        }
    }

    fn transform_fn(&mut self, fn_def: &ast::Fun, indent: usize) -> ResultC {
        let indent_s = self.indent_str(indent);
        let ret_type = if fn_def.ret.typename.len() == 0 {
            "void".into()
        } else {
            fn_def.ret.typename.clone()
        };
        let args = String::new();
        let body = self.process_ast_node(&fn_def.body, indent + INDENT_LEVEL)?;
        let c_fn = if self.scope_depth == 1 && fn_def.name == "main" {
            // this is a special case - main function
            if fn_def.ret.typename.len() != 0 {
                return Err(
                    "main function (program entry point) shall have no return type defined".into(),
                );
            }
            let args = "void";
            format!(
                "{indent_s}int {}({args}) {{{body} \n{indent_s}}}",
                fn_def.name,
            )
        } else {
            format!(
                "{indent_s}{ret_type} {}({args}) {{{body} \n{indent_s}}}",
                fn_def.name,
            )
        };
        Ok(c_fn)
    }

    fn transform_scope(&mut self, scope_nodes: &Vec<ast::Node>, indent: usize) -> ResultC {
        let mut scope_str = String::new();
        for e in scope_nodes.iter() {
            scope_str += "\n";
            scope_str += &self.process_ast_node(&e, indent)?;
        }
        Ok(scope_str)
    }

    fn transform_fn_call(&mut self, name: &str, args: &Vec<ast::Node>, indent: usize) -> ResultC {
        let indent_s = self.indent_str(indent);
        let mut args_s = String::new();
        for (i, e) in args.iter().enumerate() {
            args_s += &self.process_ast_node(&e, 0)?;
            if i < args.len() - 1 {
                args_s += ", ";
            }
        }
        let statement = if name == "print" {
            self.cstd_calls.insert("printf".into());
            format!("{indent_s}printf({args_s});")
        } else {
            format!("{indent_s}{name}({args_s});")
        };
        Ok(statement)
    }

    fn transform_if(&mut self, if_: &ast::If, indent: usize) -> ResultC {
        let cond = self.process_ast_node(&if_.cond, 0)?;
        let body = self.process_ast_node(&if_.body, indent + INDENT_LEVEL)?;
        let indent_s = self.indent_str(indent);
        let mut ret = format!("{indent_s}if ({cond}) {{{body}\n{indent_s}}}");
        // elifs?
        for elif in if_.elif.iter() {
            let cond = self.process_ast_node(&elif.cond, 0)?;
            let body = self.process_ast_node(&elif.body, indent + INDENT_LEVEL)?;
            ret += &format!(" else if ({cond}) {{\n{body}\n{indent_s}}}");
        }
        // else?
        if let Some(else_) = &if_.else_body {
            let body = self.process_ast_node(&else_, indent + INDENT_LEVEL)?;
            ret += &format!(" else {{\n{body}\n{indent_s}}}");
        }
        Ok(ret)
    }

    fn gen_include_directives(&self) -> ResultC {
        let mut stdlibs = HashSet::new();
        for fun in self.cstd_calls.iter() {
            let header = match self.fun_to_header_map.get(fun.as_str()) {
                Some(h) => h,
                None => return Err(format!("Can't find a c-header for function {fun}")),
            };
            stdlibs.insert(header);
        }
        let mut ret = String::new();
        for h in stdlibs {
            ret += &format!("#include <{h}>\n");
        }

        Ok(ret)
    }

    fn gen_bool_def(&self) -> &'static str {
        "typedef enum {false, true} kf_boolean;"
    }

    fn indent_str(&self, indent: usize) -> String {
        " ".repeat(indent)
    }
}
