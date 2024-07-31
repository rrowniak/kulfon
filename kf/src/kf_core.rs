// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::ast;
use crate::comp_msg;
use crate::comp_msg::{CompileMsgCol, TextPoint};
use crate::type_system::*;
use std::collections::HashMap;

pub struct ScopeLog {
    /// <variable name, type definition>
    var_decl: HashMap<String, KfType>,
    /// transient - can we go back and do a local variable lookup?
    /// e.g. if this is a function call - we can't
    /// e.g. if this ia a while loop - we can
    transient: bool,
}

pub struct Context {
    pub side_nodes: Vec<SideNode>,
    pub glob_functs: HashMap<String, (Vec<ast::VarDecl>, ast::TypeDecl)>,
    /// first position is a global scope with global variables and consts
    pub temp_scope: Vec<ScopeLog>,
}

impl Context {
    fn scope_push(&mut self, transient: bool) {
        self.temp_scope.push(ScopeLog {
            var_decl: HashMap::new(),
            transient,
        });
    }

    fn scope_peek(&mut self) -> &mut ScopeLog {
        let indx = self.temp_scope.len() - 1;
        self.temp_scope.get_mut(indx).unwrap()
    }

    fn scope_pop(&mut self) {
        self.temp_scope.pop();
    }

    fn scope_push_var(&mut self, name: String, details: KfType) {
        self.scope_peek().var_decl.insert(name, details);
    }

    fn scope_get_var(&mut self, name: &str) -> Option<KfType> {
        // lookup scopes up to "non-transient" on
        // lastly check the global scope
        let mut indx = self.temp_scope.len() - 1;
        while indx > 0 {
            let scope = &self.temp_scope[indx];

            if let Some(details) = scope.var_decl.get(name) {
                return Some(details.clone());
            }

            if !scope.transient {
                break;
            }
            indx -= 1;
        }
        if let Some(details) = self.temp_scope[0].var_decl.get(name) {
            Some(details.clone())
        } else {
            None
        }
    }
}

pub struct InterRepr {
    pub syntree: ast::Node,
    pub ctx: Context,
}

impl InterRepr {
    pub fn from(mut syntree: ast::Node, mut builtin: ast::Node) -> Result<Self, CompileMsgCol> {
        let mut ctx = Context {
            side_nodes: Vec::new(),
            glob_functs: HashMap::new(),
            // add a global scope
            temp_scope: Vec::new(),
        };
        // collect all function definitions
        collect_glob_functions(&mut builtin, &mut ctx)?;
        collect_glob_functions(&mut syntree, &mut ctx)?;
        // evaluate expression types
        eval_types(&mut syntree, &mut ctx)?;
        Ok(Self { syntree, ctx })
    }
}

fn collect_glob_functions(syntree: &mut ast::Node, ctx: &mut Context) -> Result<(), CompileMsgCol> {
    let mut errors = Vec::new();
    match &mut syntree.val {
        ast::Ntype::Scope(scope) => {
            for st in scope.iter_mut() {
                collect_glob_functions(st, ctx)?;
            }
        }
        ast::Ntype::FnDef(fndef) => {
            let ast::Fun {
                name,
                args,
                variadic: _,
                ret,
                body: _,
            } = fndef;
            // check if the name is correct
            if let Some(c) = name.chars().next() {
                // TODO: Add '_' to the exception list
                if !c.is_ascii_alphabetic() {
                    errors.push(comp_msg::error_inv_var_fn_name(syntree.at));
                }
            }
            if !ctx
                .glob_functs
                .insert(name.clone(), (args.clone(), ret.clone()))
                .is_none()
            {
                // collision - this function already defined
                errors.push(comp_msg::error_fn_name_in_use(syntree.at));
            }
        }
        _ => {}
    }
    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(())
    }
}

macro_rules! calculate_operator {
    ($left:expr, ==, $right:expr) => {
        Some($left == $right)
    };
    ($left:expr, $op:tt, $right:expr) => {
        match ($left, $right) {
            (EvaluatedValue::Bool(lhs), EvaluatedValue::Bool(rhs)) => Some(lhs $op rhs),
            (EvaluatedValue::Integer(lhs), EvaluatedValue::Integer(rhs)) => Some(lhs $op rhs),
            _ => None,
        }
    };
}

macro_rules! handle_logic_op {
    ($a:expr, $op:tt, $b:expr, $s:expr, $n:expr) => {{
        eval_types($a.as_mut(), $s)?;
        eval_types($b.as_mut(), $s)?;
        let _t = determine_type_for_a_b(&$a, &$b, $n.at, $s)?;
        let val = calculate_operator!(
            get_side_node(&$a, $s).unwrap().eval_val.clone().unwrap(),
            ==,
            get_side_node(&$b, $s).unwrap().eval_val.clone().unwrap()
        );
        let val = if let Some(v) = val {
            Some(EvaluatedValue::Bool(v))
        } else {
            None
        };
        set_type(
            $n,
            $s,
            KfType {
                mutable: Some(false),
                eval_type: EvaluatedType::Bool,
            },
            val,
        )
    }}
}

fn eval_types(n: &mut ast::Node, s: &mut Context) -> Result<(), CompileMsgCol> {
    match &mut n.val {
        // operators
        // binary operators:
        // * calculate a and b
        // * evaluate expression type based on a and b
        // * For logic operators resulting type is bool
        // * Try to calculate the value if possible
        ast::Ntype::Eq(a, b) => {
            eval_types(a.as_mut(), s)?;
            eval_types(b.as_mut(), s)?;
            let _t = determine_type_for_a_b(&a, &b, n.at, s)?;
            let val = None;
            // let val = calculate_operator!(
            //     get_side_node(&a, s).unwrap().eval_val.clone().unwrap(),
            //     ==,
            //     get_side_node(&b, s).unwrap().eval_val.clone().unwrap()
            // );
            // let val = if let Some(v) = val {
            //     Some(EvaluatedValue::Bool(v))
            // } else {
            //     None
            // };
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type: EvaluatedType::Bool,
                },
                val,
            )
        }
        ast::Ntype::Neq(a, b) => {
            handle_logic_op!(a, !=, b, s, n)
        }
        ast::Ntype::Gt(a, b) => {
            validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type: EvaluatedType::Bool,
                },
                None,
            )
        }
        ast::Ntype::Ge(a, b) => {
            validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type: EvaluatedType::Bool,
                },
                None,
            );
        }
        ast::Ntype::Lt(a, b) => {
            validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type: EvaluatedType::Bool,
                },
                None,
            );
        }
        ast::Ntype::Le(a, b) => {
            validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type: EvaluatedType::Bool,
                },
                None,
            );
        }
        ast::Ntype::Plus(a, b) => {
            let eval_type = validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type,
                },
                None,
            );
        }
        ast::Ntype::Minus(a, b) => {
            let eval_type = validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type,
                },
                None,
            );
        }
        ast::Ntype::Slash(a, b) => {
            let eval_type = validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type,
                },
                None,
            );
        }
        ast::Ntype::Star(a, b) => {
            let eval_type = validate_type_for_arithm_op(a, b, n.at, s)?;
            set_type(
                n,
                s,
                KfType {
                    mutable: Some(false),
                    eval_type,
                },
                None,
            );
        }
        ast::Ntype::Bang(a) => {
            eval_types(a.as_mut(), s)?;
            let asn = get_side_node(a, s).expect("Side node should be calculated here");
            let atype = asn.eval_type.eval_type.clone();
            if atype != EvaluatedType::Bool {
                return Err(vec![comp_msg::error_bool_type_expected(n.at, atype)]);
            }
        }
        ast::Ntype::UMinus(a) => {
            // unary minus
            eval_types(a.as_mut(), s)?;
            let asn = get_side_node(a, s).expect("Side node should be calculated here");
            let atype = asn.eval_type.eval_type.clone();
            if !atype.is_numeric() && !atype.is_floating() {
                return Err(vec![comp_msg::error_numeric_type_expected(n.at, atype)]);
            }
        }
        // assign
        ast::Ntype::Assign(a, b) => {
            eval_types(b.as_mut(), s)?;
            if let Some(variable) = s.scope_get_var(a) {
                if !variable.mutable.expect("Must be defined!") {
                    return Err(vec![comp_msg::error_assign_to_immutable(n.at)]);
                }
                // TODO: check types equality
            } else {
                return Err(vec![comp_msg::error_undeclared_var(n.at)]);
            }
            set_type_void(n, s);
        }
        // control flow
        ast::Ntype::If(if_) => {
            let mut errors = Vec::new();
            // condition must be bool
            eval_types(if_.cond.as_mut(), s)?;
            let asn = get_side_node(if_.cond.as_ref(), s).expect("Side node should be calculated");
            let atype = asn.eval_type.eval_type.clone();
            if atype != EvaluatedType::Bool {
                errors.push(comp_msg::error_condition_must_be_bool(n.at, "if", atype));
            }
            // evaluate body
            eval_types(if_.body.as_mut(), s)?;
            // evaluate elif
            for elif in if_.elif.iter_mut() {
                eval_types(elif.cond.as_mut(), s)?;
                let asn =
                    get_side_node(elif.cond.as_ref(), s).expect("Side node should be calculated");
                let atype = asn.eval_type.eval_type.clone();
                if atype != EvaluatedType::Bool {
                    errors.push(comp_msg::error_condition_must_be_bool(
                        n.at, "else if", atype,
                    ));
                }
                // body
                eval_types(elif.body.as_mut(), s)?;
            }
            // evaluate else
            if let Some(else_body) = &mut if_.else_body {
                eval_types(else_body.as_mut(), s)?;
            }
            if errors.len() > 0 {
                return Err(errors);
            }
            // TODO: set type?
            set_type_void(n, s);
        }
        ast::Ntype::For(for_) => {
            // TODO: handle matching patterns
            eval_types(for_.in_expr.as_mut(), s)?;
            eval_types(for_.body.as_mut(), s)?;

            set_type_void(n, s);
        }
        ast::Ntype::While(while_) => {
            let mut errors = Vec::new();
            eval_types(while_.cond.as_mut(), s)?;
            let asn =
                get_side_node(while_.cond.as_ref(), s).expect("Side node should be calculated");
            let atype = asn.eval_type.eval_type.clone();
            if atype != EvaluatedType::Bool {
                errors.push(comp_msg::error_condition_must_be_bool(n.at, "while", atype));
            }
            // evaluate body
            eval_types(while_.body.as_mut(), s)?;
            if errors.len() > 0 {
                return Err(errors);
            }

            set_type_void(n, s);
        }
        ast::Ntype::Loop(loop_) => {
            eval_types(loop_.body.as_mut(), s)?;

            set_type_void(n, s);
        }
        ast::Ntype::Break => set_type(n, s, KfType::from_literal(EvaluatedType::Never), None),
        ast::Ntype::Continue => set_type(n, s, KfType::from_literal(EvaluatedType::Never), None),
        // higher level structures
        ast::Ntype::Struct(_) => unimplemented!(),
        ast::Ntype::Enum(_) => unimplemented!(),
        ast::Ntype::Impl(_) => unimplemented!(),
        ast::Ntype::Scope(a) => {
            s.scope_push(true);
            for n in a {
                eval_types(n, s)?;
            }

            set_type_void(n, s);
            s.scope_pop();
        }
        ast::Ntype::FnDef(fndef) => {
            s.scope_push(false);
            eval_types(fndef.body.as_mut(), s)?;
            set_type_void(n, s);
            s.scope_pop();
        }
        ast::Ntype::FnCall(name, args) => {
            // evaluate all expressions passed as arguments
            for arg in args.iter_mut() {
                eval_types(arg, s)?;
            }
            // do the validation
            if let Some((args_def, ret_def)) = s.glob_functs.get(name) {
                let mut errors = Vec::new();
                if args.len() != args_def.len() {
                    let passed = args.len();
                    let expected = args_def.len();
                    errors.push(comp_msg::error_fn_call_incorrect_no_args(
                        n.at, passed, expected,
                    ));
                }
                for (arg, arg_def) in args.iter().zip(args_def) {
                    let arg_type = &get_side_node(arg, s)
                        .expect("Side node should be calculated here")
                        .eval_type
                        .eval_type;
                    // TODO:
                    let arg_type_dec =
                        EvaluatedType::from_str(&arg_def.type_dec.typename().unwrap());
                    if let Some(arg_type_dec) = arg_type_dec {
                        if *arg_type != arg_type_dec {
                            errors.push(comp_msg::error_fn_call_arg_mismatch(
                                arg.at,
                                arg_type.clone(),
                                arg_type_dec,
                            ));
                        }
                    } else {
                        // TODO: not the best place for reporting this error
                        // TODO:
                        let t = arg_def.type_dec.typename().unwrap().clone();
                        errors.push(comp_msg::error_unrecognised_type(arg.at, &t));
                    }
                }
                // set return type
                // TODO:
                let ret_type = if ret_def.typename().unwrap().len() == 0 {
                    Some(EvaluatedType::Void)
                } else {
                    // TODO:
                    EvaluatedType::from_str(&ret_def.typename().unwrap())
                };
                if let Some(ret_type) = ret_type {
                    set_type(
                        n,
                        s,
                        KfType {
                            mutable: Some(false),
                            eval_type: ret_type,
                        },
                        None,
                    );
                } else {
                    // TODO: not the best place for reporting return value type issues
                    // TODO:
                    let t = ret_def.typename().unwrap().clone();
                    errors.push(comp_msg::error_unrecognised_type(n.at, &t));
                }
                if errors.len() > 0 {
                    return Err(errors);
                }
            } else {
                return Err(vec![comp_msg::error_undefined_function(n.at, name)]);
            }
        }
        ast::Ntype::VarDef(vardef) => {
            match &mut vardef.expr {
                Some(expr) => {
                    eval_types(expr.as_mut(), s)?;
                }
                None => {}
            }
            s.scope_push_var(
                vardef.name.clone(),
                KfType {
                    mutable: Some(vardef.mutable),
                    eval_type: match &vardef.vartype {
                        // TODO:
                        Some(vd) => match EvaluatedType::from_str(&vd.typename().unwrap()) {
                            Some(eval_t) => eval_t,
                            None => EvaluatedType::ToBeInferred,
                        },
                        None => EvaluatedType::ToBeInferred,
                    },
                },
            );
            set_type_void(n, s);
        }
        // terminals
        ast::Ntype::String(ss) => {
            let ss = ss.clone();
            set_type(
                n,
                s,
                KfType::from_literal(EvaluatedType::String),
                Some(EvaluatedValue::String(ss)),
            );
        }
        ast::Ntype::Literal(lit) => {
            let (t, v) = classify_literal(&lit);
            // is it a variable name?
            if t == EvaluatedType::ToBeInferred && v == None {
                // check if a given variable was declared
                if let Some(_variable) = s.scope_get_var(&lit) {
                    // TODO: and if so, check if the type is already determined
                } else {
                    // error
                    return Err(vec![comp_msg::error_undeclared_var(n.at)]);
                }
            }
            let mutable = if v.is_some() { Some(false) } else { None };
            set_type(
                n,
                s,
                KfType {
                    mutable,
                    eval_type: t,
                },
                v,
            );
        }
        ast::Ntype::Char(c) => {
            let c = c.clone();
            set_type(
                n,
                s,
                KfType::from_literal(EvaluatedType::Char),
                Some(EvaluatedValue::Char(
                    c.chars().next().expect("Can't extract character"),
                )),
            );
        }
        ast::Ntype::True => set_type(
            n,
            s,
            KfType::from_literal(EvaluatedType::Bool),
            Some(EvaluatedValue::Bool(false)),
        ),
        ast::Ntype::False => set_type(
            n,
            s,
            KfType::from_literal(EvaluatedType::Bool),
            Some(EvaluatedValue::Bool(false)),
        ),
    }
    Ok(())
}

fn set_type(n: &mut ast::Node, ctx: &mut Context, t: KfType, v: Option<EvaluatedValue>) {
    match n.meta_idx {
        Some(idx) => {
            ctx.side_nodes[idx].eval_type = t;
            ctx.side_nodes[idx].eval_val = v;
        }
        None => {
            ctx.side_nodes.push(SideNode {
                eval_type: t,
                eval_val: v,
            });
            n.meta_idx = Some(ctx.side_nodes.len() - 1);
        }
    }
}

fn set_type_void(n: &mut ast::Node, ctx: &mut Context) {
    let t = KfType {
        mutable: None,
        eval_type: EvaluatedType::Void,
    };
    match n.meta_idx {
        Some(idx) => {
            ctx.side_nodes[idx].eval_type = t;
            ctx.side_nodes[idx].eval_val = None;
        }
        None => {
            ctx.side_nodes.push(SideNode {
                eval_type: t,
                eval_val: None,
            });
            n.meta_idx = Some(ctx.side_nodes.len() - 1);
        }
    }
}

fn get_side_node<'a>(n: &'a ast::Node, ctx: &'a Context) -> Option<&'a SideNode> {
    if let Some(side_node) = n.meta_idx {
        Some(&ctx.side_nodes[side_node])
    } else {
        None
    }
}

fn determine_type_for_a_b(
    a: &ast::Node,
    b: &ast::Node,
    at: TextPoint,
    ctx: &Context,
) -> Result<EvaluatedType, CompileMsgCol> {
    let asn = get_side_node(a, ctx).expect("Side node should be calculated here!");
    let bsn = get_side_node(b, ctx).expect("Side node should be calculated here (2)");

    // println!("asn={:?}\nbsn={:?}", asn, bsn);

    // TODO: unfortunate names - figure out better namings
    let atype = asn.eval_type.eval_type.clone();
    let btype = bsn.eval_type.eval_type.clone();

    let a_inf = atype == EvaluatedType::ToBeInferred;
    let b_inf = btype == EvaluatedType::ToBeInferred;
    // one or both migtd be "to be inferred"
    if a_inf && b_inf {
        return Ok(EvaluatedType::ToBeInferred);
    } else if a_inf {
        return Ok(btype);
    } else if b_inf {
        return Ok(atype);
    }

    let a_float = atype == EvaluatedType::FloatingNum;
    let b_float = btype == EvaluatedType::FloatingNum;
    // floating numbers
    if a_float && btype.is_floating() {
        return Ok(btype);
    } else if b_float && atype.is_floating() {
        return Ok(atype);
    }

    let a_int = atype == EvaluatedType::Integer;
    let b_int = btype == EvaluatedType::Integer;
    // integers
    if a_int && btype.is_numeric() {
        // TODO: decide if 8 & 16-bits types should be promoted to isize/usize
        return Ok(btype);
    } else if b_int && atype.is_numeric() {
        return Ok(atype);
    }

    if atype != btype {
        return Err(vec![comp_msg::error_type_mismatch_for_bin_op(
            at, atype, btype,
        )]);
    }
    Ok(atype)
}
fn validate_type_for_arithm_op(
    a: &mut ast::Node,
    b: &mut ast::Node,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<EvaluatedType, CompileMsgCol> {
    eval_types(a, ctx)?;
    eval_types(b, ctx)?;
    let t = determine_type_for_a_b(&a, &b, at, ctx)?;
    if !t.is_numeric() && !t.is_floating() {
        return Err(vec![comp_msg::error_numeric_type_for_bin_op(
            at,
            t.clone(),
            t,
        )]);
    }
    Ok(t)
}

#[derive(Debug)]
pub struct SideNode {
    pub eval_type: KfType,
    pub eval_val: Option<EvaluatedValue>,
}

fn classify_literal(input: &str) -> (EvaluatedType, Option<EvaluatedValue>) {
    // Check if it's a signed integer, now up to 64-bit only
    if let Ok(value) = input.parse::<i64>() {
        let bits = if value >= i8::MIN as i64 && value <= i8::MAX as i64 {
            8
        } else if value >= i16::MIN as i64 && value <= i16::MAX as i64 {
            16
        } else if value >= i32::MIN as i64 && value <= i32::MAX as i64 {
            32
        } else {
            64
        };
        return (
            EvaluatedType::Integer,
            Some(EvaluatedValue::Integer(EvaluatedInt {
                signed: true,
                min_bits: bits,
                val: Int::Signed(value as i128),
            })),
        );
    }

    // Check if it's an unsigned integer, now up to 64-bit only
    if let Ok(value) = input.parse::<u64>() {
        let bits = if value <= u8::MAX as u64 {
            8
        } else if value <= u16::MAX as u64 {
            16
        } else if value <= u32::MAX as u64 {
            32
        } else {
            64
        };
        return (
            EvaluatedType::Integer,
            Some(EvaluatedValue::Integer(EvaluatedInt {
                signed: false,
                min_bits: bits,
                val: Int::Unsigned(value as u128),
            })),
        );
    }

    // Check if it's a floating-point number
    if let Ok(value) = input.parse::<f64>() {
        if let Ok(_value) = input.parse::<f32>() {
            return (
                EvaluatedType::FloatingNum,
                Some(EvaluatedValue::Floating(EvaluatedFloat::F32(_value))),
            );
        } else {
            return (
                EvaluatedType::FloatingNum,
                Some(EvaluatedValue::Floating(EvaluatedFloat::F64(value))),
            );
        }
    }

    // If none of the above, treat it as a literal
    (EvaluatedType::ToBeInferred, None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler;
    use crate::lang_def::Lang;
    use crate::lexer;
    use crate::parser;

    fn throw_errors(errs: CompileMsgCol) -> Result<(), String> {
        if errs.len() == 0 {
            Ok(())
        } else {
            let mut error = String::new();
            for e in errs {
                let at = e.at.unwrap_or(TextPoint { line: 0, col: 0 });
                let msg = format!("Syntax error at {}, {}: {}", at.line, at.col, e.msg);
                error.push_str(&msg);
                error += "\n";
            }
            Err(error)
        }
    }

    fn parse_code(code: &str) -> Result<(ast::Node, ast::Node), String> {
        let (tokens, errors) = lexer::tokenize(&Lang::new(), compiler::BUILT_IN_STUFF);
        throw_errors(errors)?;
        let ast_built_in = parser::parse(&tokens).unwrap();

        let (tokens, errors) = lexer::tokenize(&Lang::new(), code);
        throw_errors(errors)?;
        let ast_tree = parser::parse(&tokens);
        match ast_tree {
            Ok(ast) => Ok((ast, ast_built_in)),
            Err(errs) => {
                let mut err_msg = String::new();
                for e in errs {
                    let at = e.at.unwrap_or(TextPoint { line: 0, col: 1 });
                    err_msg += code;
                    err_msg += &format!("\n{}^\n", " ".repeat(at.col - 1));
                    err_msg += &format!("Syntax error: {}", e.msg)
                }
                Err(err_msg)
            }
        }
    }

    #[test]
    fn test_positive_cases() {
        let tcs = [
            "let v = 1 + 2;",
            "fn ab() {}",
            "fn a() {} fn b() { a(); }",
            "fn a() { a(); }",
            "fn a() { let b: i32 = 0; if b == 0 {}}",
            "fn a() { let mut b: i32 = 0; if true {b = 1;}}",
        ];
        for c in tcs {
            let (ast, built_in) = parse_code(c).unwrap();
            let inter = InterRepr::from(ast, built_in);
            match inter {
                Ok(_) => {}
                Err(e) => {
                    println!("Parsing: {}", c);
                    for e0 in e {
                        println!("{:?}", e0);
                    }
                    assert!(false);
                }
            }
        }
    }

    #[test]
    fn test_negative_cases() {
        let tcs = [
            r#"fn a(){1 == "s"; }"#,
            "let v = 1 == \"srt\";",
            "fn a() {} fn a() {}",
            "fn 1a() {}",
            "fn a() {} fn b() {aa();}",
            "fn a(i: i32) {} fn b() {a(1, true);}",
            "fn a(i: i32) {} fn b() {a(true);}",
            "fn a() { let b: i32 = 0; if true {b = 1;}}",
        ];
        for c in tcs {
            let (ast, built_in) = parse_code(c).unwrap();
            let inter = InterRepr::from(ast, built_in);
            match inter {
                Ok(int) => {
                    println!("code: {c}");
                    println!("ast: {:?}", int.syntree);
                    assert!(false);
                }
                Err(_) => {}
            }
        }
    }
}
