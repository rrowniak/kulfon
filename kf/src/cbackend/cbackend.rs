// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::ast;
use crate::cbackend::type_conv;
use crate::kf_core;
use crate::type_system;
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
    stdlibs: HashSet<&'static str>,
    loc_variables: Vec<Vec<ast::VarDef>>,
}

impl<'a> CGen<'a> {
    fn new(mid: &'a kf_core::InterRepr) -> Self {
        Self {
            mid,
            scope_depth: 0,
            cstd_calls: HashSet::new(),
            fun_to_header_map: HashMap::from([("printf", "stdio.h")]),
            bool_in_use: false,
            stdlibs: HashSet::new(),
            loc_variables: Vec::new(),
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
            ast::Ntype::Break => Ok(format!("{}break;", self.indent_str(indent))),
            ast::Ntype::Continue => Ok(format!("{}continue;", self.indent_str(indent))),
            ast::Ntype::Struct(_) => unimplemented!(),
            ast::Ntype::Enum(_) => unimplemented!(),
            ast::Ntype::Impl(_) => unimplemented!(),
            ast::Ntype::False => {
                self.bool_in_use = true;
                Ok(format!("false"))
            }
            ast::Ntype::True => {
                self.bool_in_use = true;
                Ok(format!("true"))
            }
            ast::Ntype::If(if_) => self.transform_if(if_, indent),
            ast::Ntype::For(for_) => self.transform_for(for_, indent),
            ast::Ntype::While(while_) => self.transform_while(while_, indent),
            ast::Ntype::Loop(loop_) => self.transform_loop(loop_, indent),
            ast::Ntype::VarDef(var) => {
                match self.loc_variables.last_mut() {
                    Some(v) => v.push(var.clone()),
                    None => panic!("No stack for this scope!"),
                }
                Ok("".into())
            }
            ast::Ntype::Assign(left, right) => Ok(format!(
                "{}{} = {};",
                self.indent_str(indent),
                left,
                self.process_ast_node(right, 0)?
            )),
            ast::Ntype::String(s) => Ok(format!("{}\"{}\"", self.indent_str(indent), s)),
            ast::Ntype::Char(s) => Ok(format!("'{s}'")),
            ast::Ntype::Literal(s) => Ok(format!("{}{}", self.indent_str(indent), s)),
            // other => Err(format!("C backend: unsupported expression: {:?}", other)),
        }
    }

    fn transform_fn(&mut self, fn_def: &ast::Fun, indent: usize) -> ResultC {
        let indent_s = self.indent_str(indent);
        // TODO:
        let ret_type = if fn_def.ret.typename().unwrap().len() == 0 {
            "void".into()
        } else {
            // TODO:
            fn_def.ret.typename().unwrap().clone()
        };
        let args = String::new();
        let body = self.process_ast_node(&fn_def.body, indent + INDENT_LEVEL)?;
        let c_fn = if self.scope_depth == 1 && fn_def.name == "main" {
            // this is a special case - main function
            if fn_def.ret.typename().unwrap().len() != 0 {
                return Err(
                    "main function (program entry point) shall have no return type defined".into(),
                );
            }
            let args = "void";
            let indent_ret = self.indent_str(indent + INDENT_LEVEL);
            format!(
                "{indent_s}int {}({args}) {{{body} \n{indent_ret}return 0;\n{indent_s}}}",
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
        self.loc_variables.push(Vec::new());
        let mut scope_str = String::new();
        for e in scope_nodes.iter() {
            scope_str += "\n";
            scope_str += &self.process_ast_node(&e, indent)?;
        }
        let mut loc_vars = String::new();
        let loc_scope_vars = self.loc_variables.pop().unwrap();
        for v in loc_scope_vars {
            loc_vars += "\n";
            loc_vars += self.transform_var_def(&v, indent)?.as_str();
            loc_vars += ";";
        }
        Ok(format!("{loc_vars}{scope_str}"))
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

    fn transform_for(&mut self, _for_: &ast::For, _indent: usize) -> ResultC {
        todo!()
    }

    fn transform_while(&mut self, while_: &ast::While, indent: usize) -> ResultC {
        let cond = self.process_ast_node(&while_.cond, 0)?;
        let body = self.process_ast_node(&while_.body, indent + INDENT_LEVEL)?;
        let indent_s = self.indent_str(indent);
        let ret = format!("{indent_s}while ({cond}) {{{body}\n{indent_s}}}");
        Ok(ret)
    }

    fn transform_loop(&mut self, loop_: &ast::Loop, indent: usize) -> ResultC {
        let body = self.process_ast_node(&loop_.body, indent + INDENT_LEVEL)?;
        let indent_s = self.indent_str(indent);
        self.bool_in_use = true;
        let ret = format!("{indent_s}while (true) {{{body}\n{indent_s}}}");
        Ok(ret)
    }

    fn transform_var_def(&mut self, var: &ast::VarDef, indent: usize) -> ResultC {
        let mut ret = String::new();
        let indent_s = self.indent_str(indent);
        ret += indent_s.as_str();
        if !var.mutable {
            ret += "const ";
        }
        let varname = &var.name;
        let type_kf = var
            .vartype
            .clone()
            .expect(&format!("Type for {varname} must be deduced"));
        // TODO:
        if let Some(kf_built_in) =
            type_system::EvaluatedType::from_str(&type_kf.typename().unwrap())
        {
            // this is a built-in KF type, convert it to C type
            let type_c = type_conv::convert_built_in(kf_built_in.clone()).expect(&format!(
                "Fatal error while convertig KF built-in type {kf_built_in:?} into C-type",
            ));
            if kf_built_in == type_system::EvaluatedType::Bool {
                self.bool_in_use = true;
            } else {
                self.stdlibs.extend(type_conv::required_headers());
            }
            ret += format!("{type_c} {varname}").as_str();
        } else {
            // presumably a custom type
            todo!();
        }

        let var_expr = var.expr.clone().expect("Uninitialized variable detected");
        ret += format!(" = {}", self.process_ast_node(&var_expr, 0)?).as_str();

        Ok(ret)
    }

    fn gen_include_directives(&self) -> ResultC {
        let mut stdlibs: HashSet<&'static str> = HashSet::new();
        for fun in self.cstd_calls.iter() {
            let header = match self.fun_to_header_map.get(fun.as_str()) {
                Some(h) => h,
                None => return Err(format!("Can't find a c-header for function {fun}")),
            };
            stdlibs.insert(header);
        }
        let mut ret = String::new();
        for h in stdlibs.union(&self.stdlibs) {
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
