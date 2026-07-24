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
    pub tagged_enums: HashSet<String>,
}

impl CGenCtx {
    pub(crate) fn new() -> Self {
        Self {
            stdlibs: HashSet::new(),
            cstd_calls: HashSet::new(),
            bool_in_use: false,
            tagged_enums: HashSet::new(),
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
    loc_variables: Vec<Vec<(kf_core::SideNodeRef, ast::VarDef)>>,
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
        // populate tagged_enums lookup
        for (name, decl) in &self.mid.ctx.glob_enums {
            if decl.variants.iter().any(|(_, p)| p.is_some()) {
                self.ctx.tagged_enums.insert(name.clone());
            }
        }
        let code = self.process_ast_node(&self.mid.syntree, self.mid.syntree.root, 0)?;
        // generate struct/enum defs first to collect stdlib requirements
        let mut struct_defs = String::new();
        for name in &self.mid.ctx.struct_order {
            if let Some(decl) = self.mid.ctx.glob_structs.get(name) {
                struct_defs += &generators::gen_struct_def(name, &decl.fields, &mut self.ctx);
            }
        }
        let mut enum_defs = String::new();
        for (name, decl) in &self.mid.ctx.glob_enums {
            enum_defs += &generators::gen_enum_def(name, &decl.variants, &mut self.ctx);
        }
        // generate includes after struct/enum processing (they may add stdlib requirements)
        let mut ret = self.ctx.generate_context_code();
        ret += &struct_defs;
        ret += &enum_defs;
        // generate forward declarations for user-defined functions only
        let builtins = ["print", "println", "rand", "assert", "panic", "exit", "sleep", "env", "file_to_str", "file_to_bytes", "file_exists"];
        for (name, decl) in &self.mid.ctx.glob_functs {
            if name == "main" || builtins.contains(&name.as_str()) {
                continue;
            }
            let ret_str = decl.ret.typename().unwrap_or_default();
            let ret_type = if ret_str.is_empty() {
                String::from("void")
            } else {
                let eval_type = type_system::EvaluatedType::from_str(&ret_str)
                    .and_then(|et| generators::generate_type(et, &mut self.ctx));
                match eval_type {
                    Some(c_type) => String::from(c_type),
                    None => continue,
                }
            };
            let args: Vec<String> = decl
                .args
                .iter()
                .filter_map(|arg| {
                    let type_name = arg.type_dec.typename()?;
                    let eval_type = type_system::EvaluatedType::from_str(&type_name)?;
                    let c_type = generators::generate_type(eval_type, &mut self.ctx)?;
                    Some(format!("{c_type} {}", arg.name))
                })
                .collect();
            let args_str = if args.is_empty() {
                if decl.variadic { "...".into() } else { "void".into() }
            } else if decl.variadic {
                format!("{}, ...", args.join(", "))
            } else {
                args.join(", ")
            };
            ret += &format!("{ret_type} {name}({args_str});\n");
        }
        ret += code.as_str();
        Ok(ret)
    }

    fn process_ast_node(&mut self, tree: &ast::Tree, expr: ast::NodeRef, indent: usize) -> ResultC {
        match &tree.get(expr).val {
            ast::Ntype::Scope(scope) => {
                self.scope_depth += 1;
                let r = self.transform_scope(tree, &scope, indent);
                self.scope_depth -= 1;
                r
            }
            ast::Ntype::FnDef(fn_def) => self.transform_fn(tree, &fn_def, indent),
            ast::Ntype::FnCall(fn_name, fn_args) => {
                self.transform_fn_call(tree, &fn_name, &fn_args, indent)
            }
            ast::Ntype::StructInit(name, fields) => {
                self.transform_struct_init(tree, name, fields, indent)
            }
            ast::Ntype::EnumInit(type_name, variant, payload) => {
                self.transform_enum_init(tree, type_name, variant, *payload, indent)
            }
            ast::Ntype::FieldAccess(base, field_name) => {
                let base_s = self.process_ast_node(tree, *base, 0)?;
                Ok(format!("{}.{}", base_s, field_name))
            }
            ast::Ntype::Eq(a, b) => Ok(format!(
                "{}{} == {}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Neq(a, b) => Ok(format!(
                "{}{} != {}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Gt(a, b) => Ok(format!(
                "{}{} > {}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Ge(a, b) => Ok(format!(
                "{}{} >= {}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Lt(a, b) => Ok(format!(
                "{}{} < {}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Le(a, b) => Ok(format!(
                "{}{} <= {}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Plus(a, b) => Ok(format!(
                "{}({} + {})",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Minus(a, b) => Ok(format!(
                "{}({} - {})",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Slash(a, b) => Ok(format!(
                "{}({} / {})",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Star(a, b) => Ok(format!(
                "{}({} * {})",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
                self.process_ast_node(tree, *b, 0)?
            )),
            ast::Ntype::Bang(a) => Ok(format!(
                "{}!{}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
            )),
            ast::Ntype::UMinus(a) => Ok(format!(
                "{}-{}",
                self.indent_str(indent),
                self.process_ast_node(tree, *a, 0)?,
            )),
            ast::Ntype::Break => Ok(format!("{}break;", self.indent_str(indent))),
            ast::Ntype::Continue => Ok(format!("{}continue;", self.indent_str(indent))),
            ast::Ntype::Return(ref expr) => {
                let indent_s = self.indent_str(indent);
                if let Some(expr) = expr {
                    let val = self.process_ast_node(tree, *expr, 0)?;
                    Ok(format!("{indent_s}return {val};"))
                } else {
                    Ok(format!("{indent_s}return;"))
                }
            }
            ast::Ntype::Struct(_) => Ok("".into()),
            ast::Ntype::Enum(_) => Ok("".into()),
            ast::Ntype::Impl(_) => Ok("".into()),
            ast::Ntype::False => Ok(generators::gen_bool_var(false, &mut self.ctx).into()),
            ast::Ntype::True => Ok(generators::gen_bool_var(true, &mut self.ctx).into()),
            ast::Ntype::If(if_) => self.transform_if(tree, &if_, indent),
            ast::Ntype::For(for_) => self.transform_for(tree, &for_, indent),
            ast::Ntype::While(while_) => self.transform_while(tree, &while_, indent),
            ast::Ntype::Loop(loop_) => self.transform_loop(tree, &loop_, indent),
            ast::Ntype::VarDef(var) => {
                match self.loc_variables.last_mut() {
                    Some(v) => v.push((kf_core::SideNodeRef::from_node_ref(expr), var.clone())),
                    None => panic!("No stack for this scope!"),
                }
                Ok("".into())
            }
            ast::Ntype::Assign(left, right) => Ok(format!(
                "{}{} = {};",
                self.indent_str(indent),
                left,
                self.process_ast_node(tree, *right, 0)?
            )),
            ast::Ntype::String(s) => Ok(format!("{}\"{}\"", self.indent_str(indent), s)),
            ast::Ntype::Char(s) => Ok(format!("'{s}'")),
            ast::Ntype::Literal(s) => Ok(format!("{}{}", self.indent_str(indent), s)),
            // other => Err(format!("C backend: unsupported expression: {:?}", other)),
        }
    }

    fn transform_fn(&mut self, tree: &ast::Tree, fn_def: &ast::Fun, indent: usize) -> ResultC {
        let indent_s = self.indent_str(indent);
        let ret_type = if fn_def.ret.typename().unwrap().len() == 0 {
            String::from("void")
        } else {
            let type_name = fn_def.ret.typename().unwrap();
            let eval_type = type_system::EvaluatedType::from_str(&type_name);
            match eval_type.and_then(|et| generators::generate_type(et, &mut self.ctx)) {
                Some(c_type) => String::from(c_type),
                None => return Err(format!("Unknown return type: {}", type_name).into()),
            }
        };
        let args = fn_def
            .args
            .iter()
            .filter_map(|arg| {
                let type_name = arg.type_dec.typename()?;
                let eval_type = type_system::EvaluatedType::from_str(&type_name)?;
                let c_type = generators::generate_type(eval_type, &mut self.ctx)?;
                Some(format!("{c_type} {}", arg.name))
            })
            .collect::<Vec<_>>()
            .join(", ");
        let args = if args.is_empty() {
            if fn_def.variadic { "...".into() } else { "void".into() }
        } else if fn_def.variadic {
            format!("{args}, ...")
        } else {
            args
        };
        let body = self.process_ast_node(tree, fn_def.body, indent + INDENT_LEVEL)?;
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

    fn transform_scope(
        &mut self,
        tree: &ast::Tree,
        scope_nodes: &Vec<ast::NodeRef>,
        indent: usize,
    ) -> ResultC {
        self.loc_variables.push(Vec::new());
        let mut scope_str = String::new();
        for e in scope_nodes.iter() {
            scope_str += "\n";
            let node_val = &tree.get(*e).val;
            let node_s = self.process_ast_node(tree, *e, indent)?;
            match node_val {
                ast::Ntype::FnCall(..) => scope_str += &format!("{node_s};"),
                _ => scope_str += &node_s,
            }
        }
        let mut loc_vars = String::new();
        let loc_scope_vars = self.loc_variables.pop().unwrap();
        for (i, v) in loc_scope_vars {
            loc_vars += "\n";
            loc_vars += self.transform_var_def(tree, &v, i, indent)?.as_str();
            loc_vars += ";";
        }
        Ok(format!("{loc_vars}{scope_str}"))
    }

    fn transform_fn_call(
        &mut self,
        tree: &ast::Tree,
        name: &str,
        args: &Vec<ast::NodeRef>,
        indent: usize,
    ) -> ResultC {
        let indent_s = self.indent_str(indent);
        let mut args_v = Vec::new();
        for e in args.iter() {
            let arg_s = self.process_ast_node(tree, *e, 0)?;
            let side_node = kf_core::get_side_node(*e, &self.mid.ctx);
            let eval_t = side_node.eval_type.clone();
            let eval_v = side_node.eval_val.clone();
            args_v.push((arg_s, eval_t, eval_v));
        }
        let fn_call_s = generators::gen_fn_call(name, &args_v, &mut self.ctx);
        Ok(format!("{indent_s}{fn_call_s}"))
    }

    fn transform_struct_init(
        &mut self,
        tree: &ast::Tree,
        _name: &str,
        fields: &Vec<(String, ast::NodeRef)>,
        indent: usize,
    ) -> ResultC {
        let indent_s = self.indent_str(indent);
        let mut field_strs = Vec::new();
        for (_fname, expr) in fields.iter() {
            let val = self.process_ast_node(tree, *expr, 0)?;
            field_strs.push(val);
        }
        let fields_s = field_strs.join(", ");
        Ok(format!("{indent_s}{{{fields_s}}}"))
    }

    fn transform_enum_init(
        &mut self,
        tree: &ast::Tree,
        type_name: &str,
        variant: &str,
        payload: Option<ast::NodeRef>,
        indent: usize,
    ) -> ResultC {
        let indent_s = self.indent_str(indent);
        if let Some(expr) = payload {
            let val = self.process_ast_node(tree, expr, 0)?;
            // check if this enum has payloads (tagged union) or not
            let is_tagged = self.mid.ctx.glob_enums.get(type_name)
                .map(|decl| decl.variants.iter().any(|(_, p)| p.is_some()))
                .unwrap_or(false);
            if is_tagged {
                Ok(format!("{indent_s}{{ {type_name}_{variant}, {{ {val} }} }}"))
            } else {
                Ok(format!("{indent_s}{type_name}_{variant}"))
            }
        } else {
            Ok(format!("{indent_s}{type_name}_{variant}"))
        }
    }

    fn transform_if(&mut self, tree: &ast::Tree, if_: &ast::If, indent: usize) -> ResultC {
        let cond = self.process_ast_node(tree, if_.cond, 0)?;
        let body = self.process_ast_node(tree, if_.body, indent + INDENT_LEVEL)?;
        let indent_s = self.indent_str(indent);
        let mut ret = format!("{indent_s}if ({cond}) {{{body}\n{indent_s}}}");
        // elifs?
        for elif in if_.elif.iter() {
            let cond = self.process_ast_node(tree, elif.cond, 0)?;
            let body = self.process_ast_node(tree, elif.body, indent + INDENT_LEVEL)?;
            ret += &format!(" else if ({cond}) {{\n{body}\n{indent_s}}}");
        }
        // else?
        if let Some(else_) = &if_.else_body {
            let body = self.process_ast_node(tree, *else_, indent + INDENT_LEVEL)?;
            ret += &format!(" else {{\n{body}\n{indent_s}}}");
        }
        Ok(ret)
    }

    fn transform_for(&mut self, _tree: &ast::Tree, _for_: &ast::For, _indent: usize) -> ResultC {
        todo!()
    }

    fn transform_while(&mut self, tree: &ast::Tree, while_: &ast::While, indent: usize) -> ResultC {
        let cond = self.process_ast_node(tree, while_.cond, 0)?;
        let body = self.process_ast_node(tree, while_.body, indent + INDENT_LEVEL)?;
        let indent_s = self.indent_str(indent);
        let ret = format!("{indent_s}while ({cond}) {{{body}\n{indent_s}}}");
        Ok(ret)
    }

    fn transform_loop(&mut self, tree: &ast::Tree, loop_: &ast::Loop, indent: usize) -> ResultC {
        let body = self.process_ast_node(tree, loop_.body, indent + INDENT_LEVEL)?;
        let indent_s = self.indent_str(indent);
        let ret = format!(
            "{indent_s}while ({}) {{{body}\n{indent_s}}}",
            generators::gen_bool_var(true, &mut self.ctx)
        );
        Ok(ret)
    }

    fn transform_var_def(
        &mut self,
        tree: &ast::Tree,
        var: &ast::VarDef,
        side_node_indx: kf_core::SideNodeRef,
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
            self.mid
                .ctx
                .get_side_node(side_node_indx)
                .eval_type
                .eval_type
                .clone()
        };
        let type_c = generators::generate_type(kf_built_in.clone(), &mut self.ctx).expect(
            &format!("Fatal error while convertig KF built-in type {kf_built_in:?} into C-type",),
        );
        ret += format!("{type_c} {varname}").as_str();

        let var_expr = var.expr.clone().expect("Uninitialized variable detected");
        ret += format!(" = {}", self.process_ast_node(tree, var_expr, 0)?).as_str();

        Ok(ret)
    }

    fn indent_str(&self, indent: usize) -> String {
        " ".repeat(indent)
    }
}
