// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::ast;
use crate::cbackend::generators;
use crate::kf_core;
use crate::type_system;
use std::collections::{HashMap, HashSet};

pub type ResultC = Result<String, String>;
const INDENT_LEVEL: usize = 4;

pub fn gen_c_code(mid: &kf_core::InterRepr) -> ResultC {
    CGen::new(mid).gen()
}

pub struct CGenCtx {
    pub stdlibs: HashSet<&'static str>,
    pub cstd_calls: HashSet<String>,
    pub bool_in_use: bool,
}

impl CGenCtx {
    fn new() -> Self {
        Self {
            stdlibs: HashSet::new(),
            cstd_calls: HashSet::new(),
            bool_in_use: false,
        }
    }

    fn generate_context_code(&self) -> String {
        let mut ret = String::new();
        ret += &self.gen_include_directives();
        ret += "\n";
        if self.bool_in_use {
            ret += generators::gen_bool_def();
            ret += "\n";
        }
        ret
    }

    fn gen_include_directives(&self) -> String {
        let fun_to_header_map = HashMap::from([("printf", "stdio.h")]);
        let mut stdlibs: HashSet<&'static str> = HashSet::new();
        for fun in self.cstd_calls.iter() {
            let header = match fun_to_header_map.get(fun.as_str()) {
                Some(h) => h,
                None => panic!("Can't find a c-header for function {fun}"),
            };
            stdlibs.insert(header);
        }
        let mut ret = String::new();
        for h in stdlibs.union(&self.stdlibs) {
            ret += &format!("#include <{h}>\n");
        }

        ret
    }
}

struct CGen<'a> {
    mid: &'a kf_core::InterRepr,
    ctx: CGenCtx,
    scope_depth: usize,
    loc_variables: Vec<Vec<(usize, ast::VarDef)>>,
}

impl<'a> CGen<'a> {
    fn new(mid: &'a kf_core::InterRepr) -> Self {
        Self {
            mid,
            ctx: CGenCtx::new(),
            scope_depth: 0,
            loc_variables: Vec::new(),
        }
    }

    fn gen(&mut self) -> ResultC {
        let code = self.process_ast_node(&self.mid.syntree, 0)?;
        let mut ret = self.ctx.generate_context_code();
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
            ast::Ntype::False => Ok(generators::gen_bool_var(false, &mut self.ctx).into()),
            ast::Ntype::True => Ok(generators::gen_bool_var(true, &mut self.ctx).into()),
            ast::Ntype::If(if_) => self.transform_if(if_, indent),
            ast::Ntype::For(for_) => self.transform_for(for_, indent),
            ast::Ntype::While(while_) => self.transform_while(while_, indent),
            ast::Ntype::Loop(loop_) => self.transform_loop(loop_, indent),
            ast::Ntype::VarDef(var) => {
                match self.loc_variables.last_mut() {
                    Some(v) => v.push((expr.meta_idx.unwrap(), var.clone())),
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
        for (i, v) in loc_scope_vars {
            loc_vars += "\n";
            loc_vars += self.transform_var_def(&v, i, indent)?.as_str();
            loc_vars += ";";
        }
        Ok(format!("{loc_vars}{scope_str}"))
    }

    fn transform_fn_call(&mut self, name: &str, args: &Vec<ast::Node>, indent: usize) -> ResultC {
        let indent_s = self.indent_str(indent);
        let mut args_v = Vec::new();
        for e in args.iter() {
            let arg_s = self.process_ast_node(&e, 0)?;
            let side_node = kf_core::get_side_node(e, &self.mid.ctx).unwrap();
            let eval_t = side_node.eval_type.clone();
            let eval_v = side_node.eval_val.clone();
            args_v.push((arg_s, eval_t, eval_v));
        }
        let fn_call_s = generators::gen_fn_call(name, &args_v, &mut self.ctx);
        let statement = format!("{indent_s}{fn_call_s};");
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
        let ret = format!(
            "{indent_s}while ({}) {{{body}\n{indent_s}}}",
            generators::gen_bool_var(true, &mut self.ctx)
        );
        Ok(ret)
    }

    fn transform_var_def(
        &mut self,
        var: &ast::VarDef,
        side_node_indx: usize,
        indent: usize,
    ) -> ResultC {
        let mut ret = String::new();
        let indent_s = self.indent_str(indent);
        ret += indent_s.as_str();
        if !var.mutable {
            ret += "const ";
        }
        let varname = &var.name;
        let kf_built_in = if let Some(vardef_type) = &var.vartype {
            match vardef_type.type_id {
                ast::TypeKind::JustName(ref s) => type_system::EvaluatedType::from_str(s)
                    .expect("Should be possible to evaluate a type"),
                _ => todo!(),
            }
        } else {
            self.mid.ctx.side_nodes[side_node_indx]
                .eval_type
                .eval_type
                .clone()
        };
        let type_c = generators::generate_type(kf_built_in.clone(), &mut self.ctx).expect(
            &format!("Fatal error while convertig KF built-in type {kf_built_in:?} into C-type",),
        );
        ret += format!("{type_c} {varname}").as_str();

        let var_expr = var.expr.clone().expect("Uninitialized variable detected");
        ret += format!(" = {}", self.process_ast_node(&var_expr, 0)?).as_str();

        Ok(ret)
    }

    fn indent_str(&self, indent: usize) -> String {
        " ".repeat(indent)
    }
}
