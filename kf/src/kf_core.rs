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
    var_decl: HashMap<String, VarDefinition>,
    /// transient - can we go back and do a local variable lookup?
    /// e.g. if this is a function call - we can't
    /// e.g. if this ia a while loop - we can
    transient: bool,
}

#[derive(Clone)]
pub struct VarDefinition {
    v_type: KfType,
    def_at: TextPoint,
}

pub struct FunDeclaration {
    args: Vec<ast::VarDecl>,
    variadic: bool,
    ret: ast::TypeDecl,
}

pub struct Context {
    pub side_nodes: Vec<SideNode>,
    pub glob_functs: HashMap<String, FunDeclaration>,
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

    fn scope_pop(&mut self) -> Vec<(String, VarDefinition)> {
        // loop over local variables and collect those
        // for which the types weren't deduced
        let not_deduced = self
            .scope_peek()
            .var_decl
            .iter()
            .filter(|(_, v)| !v.v_type.eval_type.is_concrete())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<Vec<(String, VarDefinition)>>();
        self.temp_scope.pop();
        not_deduced
    }

    fn scope_push_var(&mut self, name: String, details: VarDefinition) {
        self.scope_peek().var_decl.insert(name, details);
    }

    fn scope_get_var(&mut self, name: &str) -> Option<VarDefinition> {
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

    fn scope_get_var_mut(&mut self, name: &str) -> Option<&mut VarDefinition> {
        // lookup scopes up to "non-transient" on
        // lastly check the global scope
        let mut indx = self.temp_scope.len() - 1;
        while indx > 0 {
            if self.temp_scope[indx].var_decl.get_mut(name).is_some() {
                return self.temp_scope[indx].var_decl.get_mut(name);
            }

            if !self.temp_scope[indx].transient {
                break;
            }
            indx -= 1;
        }
        if let Some(details) = self.temp_scope[0].var_decl.get_mut(name) {
            Some(details)
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
                variadic,
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
                .insert(
                    name.clone(),
                    FunDeclaration {
                        args: args.clone(),
                        variadic: *variadic,
                        ret: ret.clone(),
                    },
                )
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

fn eval_types(n: &mut ast::Node, ctx: &mut Context) -> Result<(), CompileMsgCol> {
    match n.val {
        // operators
        // binary operators:
        // * calculate a and b
        // * evaluate expression type based on a and b
        // * For logic operators resulting type is bool
        // * Try to calculate the value if possible
        ast::Ntype::Eq(ref mut a, ref mut b) => {
            calc_type_for_bin_nontransient_op(a.as_mut(), b.as_mut(), &mut n.meta_idx, n.at, ctx)?;
        }
        ast::Ntype::Neq(ref mut a, ref mut b) => {
            calc_type_for_bin_nontransient_op(a.as_mut(), b.as_mut(), &mut n.meta_idx, n.at, ctx)?;
        }
        ast::Ntype::Gt(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_nontransient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Ge(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_nontransient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Lt(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_nontransient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Le(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_nontransient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Plus(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_transient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Minus(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_transient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Slash(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_transient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Star(ref mut a, ref mut b) => {
            calc_type_for_bin_arithmetic_transient_op(
                a.as_mut(),
                b.as_mut(),
                &mut n.meta_idx,
                n.at,
                ctx,
            )?;
        }
        ast::Ntype::Bang(ref mut a) => {
            calc_type_for_unary_logic_op(a.as_mut(), &mut n.meta_idx, n.at, ctx)?;
        }
        ast::Ntype::UMinus(ref mut a) => {
            // unary minus
            calc_type_for_unary_arithm_op(a.as_mut(), &mut n.meta_idx, n.at, ctx)?;
        }
        // assign
        ast::Ntype::Assign(ref mut a, ref mut b) => {
            calc_type_for_assign_op(&a, b.as_mut(), &mut n.meta_idx, n.at, ctx)?;
        }
        // control flow
        ast::Ntype::If(ref mut if_) => {
            // condition must be bool
            calc_type_for_bool_cond(if_.cond.as_mut(), &mut n.meta_idx, n.at, ctx)?;
            // evaluate body
            eval_types(if_.body.as_mut(), ctx)?;
            // evaluate elif
            for elif in if_.elif.iter_mut() {
                calc_type_for_bool_cond(elif.cond.as_mut(), &mut n.meta_idx, n.at, ctx)?;
                // body
                eval_types(elif.body.as_mut(), ctx)?;
            }
            // evaluate else
            if let Some(else_body) = &mut if_.else_body {
                eval_types(else_body.as_mut(), ctx)?;
            }
            set_type_void(&mut n.meta_idx, ctx);
        }
        ast::Ntype::For(ref mut for_) => {
            // TODO: handle matching patterns
            eval_types(for_.in_expr.as_mut(), ctx)?;
            eval_types(for_.body.as_mut(), ctx)?;
            set_type_void(&mut n.meta_idx, ctx);
        }
        ast::Ntype::While(ref mut while_) => {
            calc_type_for_bool_cond(while_.cond.as_mut(), &mut n.meta_idx, n.at, ctx)?;
            // evaluate body
            eval_types(while_.body.as_mut(), ctx)?;
            set_type_void(&mut n.meta_idx, ctx);
        }
        ast::Ntype::Loop(ref mut loop_) => {
            eval_types(loop_.body.as_mut(), ctx)?;
            set_type_void(&mut n.meta_idx, ctx);
        }
        ast::Ntype::Break => set_type(
            &mut n.meta_idx,
            ctx,
            KfType::from_literal(EvaluatedType::Never),
            None,
        ),
        ast::Ntype::Continue => set_type(
            &mut n.meta_idx,
            ctx,
            KfType::from_literal(EvaluatedType::Never),
            None,
        ),
        // higher level structures
        ast::Ntype::Struct(_) => unimplemented!(),
        ast::Ntype::Enum(_) => unimplemented!(),
        ast::Ntype::Impl(_) => unimplemented!(),
        ast::Ntype::Scope(ref mut a) => {
            ctx.scope_push(true);
            for n in a.iter_mut() {
                eval_types(n, ctx)?;
            }
            set_type_void(&mut n.meta_idx, ctx);
            let not_deduced_err = ctx
                .scope_pop()
                .iter()
                .map(|(n, t)| comp_msg::error_var_type_not_deduced(t.def_at, n))
                .collect::<CompileMsgCol>();
            if not_deduced_err.len() > 0 {
                return Err(not_deduced_err);
            }
        }
        ast::Ntype::FnDef(ref mut fndef) => {
            ctx.scope_push(false);
            eval_types(fndef.body.as_mut(), ctx)?;
            set_type_void(&mut n.meta_idx, ctx);
            let not_deduced_err = ctx
                .scope_pop()
                .iter()
                .map(|(n, t)| comp_msg::error_var_type_not_deduced(t.def_at, n))
                .collect::<CompileMsgCol>();
            if not_deduced_err.len() > 0 {
                return Err(not_deduced_err);
            }
        }
        ast::Ntype::FnCall(ref mut name, ref mut args) => {
            calc_type_for_fn_call(&name, args, &mut n.meta_idx, n.at, ctx)?;
        }
        ast::Ntype::VarDef(ref mut vardef) => {
            calc_type_for_var_def(vardef, &mut n.meta_idx, n.at, ctx)?;
        }
        // terminals
        ast::Ntype::String(ref mut ss) => {
            let ss = ss.clone();
            set_type(
                &mut n.meta_idx,
                ctx,
                KfType::from_literal(EvaluatedType::String),
                Some(EvaluatedValue::String(ss)),
            );
        }
        ast::Ntype::Literal(ref mut lit) => {
            calc_type_for_literal(&lit, &mut n.meta_idx, n.at, ctx)?;
        }
        ast::Ntype::Char(ref mut c) => {
            let c = c.clone();
            set_type(
                &mut n.meta_idx,
                ctx,
                KfType::from_literal(EvaluatedType::Char),
                Some(EvaluatedValue::Char(
                    c.chars().next().expect("Can't extract character"),
                )),
            );
        }
        ast::Ntype::True => set_type(
            &mut n.meta_idx,
            ctx,
            KfType::from_literal(EvaluatedType::Bool),
            Some(EvaluatedValue::Bool(false)),
        ),
        ast::Ntype::False => set_type(
            &mut n.meta_idx,
            ctx,
            KfType::from_literal(EvaluatedType::Bool),
            Some(EvaluatedValue::Bool(false)),
        ),
    }
    Ok(())
}
///
/// Calculate the types for binary logic operators
/// where types are not propagated up:
/// a:type1 OP b:type1 ==> bool
///
fn calc_type_for_bin_nontransient_op(
    a: &mut ast::Node,
    b: &mut ast::Node,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(a, ctx)?;
    eval_types(b, ctx)?;
    let t = determine_type_for_a_b(&a, &b, at, ctx)?;
    // println!("type ab = {t:?}");
    set_type_all_way_down(t.clone(), &a, ctx)?;
    set_type_all_way_down(t, &b, ctx)?;
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
        parent,
        ctx,
        KfType {
            mutable: Some(false),
            eval_type: EvaluatedType::Bool,
        },
        val,
    );

    Ok(())
}
///
/// Calculate the types for binary arithmetic operator
/// where types are not propagated up:
/// a:type1 OP b:type1 ==> bool
///
fn calc_type_for_bin_arithmetic_nontransient_op(
    a: &mut ast::Node,
    b: &mut ast::Node,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
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

    set_type_all_way_down(t.clone(), &a, ctx)?;
    set_type_all_way_down(t.clone(), &b, ctx)?;
    set_type(
        parent,
        ctx,
        KfType {
            mutable: Some(false),
            eval_type: EvaluatedType::Bool,
        },
        None,
    );
    Ok(())
}
///
/// Calculate the types for binary arithmetic operator
/// where types are propagated up:
/// a:type1 OP b:type1 ==> type1
///
fn calc_type_for_bin_arithmetic_transient_op(
    a: &mut ast::Node,
    b: &mut ast::Node,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
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
    set_type_all_way_down(t.clone(), &a, ctx)?;
    set_type_all_way_down(t.clone(), &b, ctx)?;
    set_type(
        parent,
        ctx,
        KfType {
            mutable: Some(false),
            eval_type: t,
        },
        None,
    );
    Ok(())
}
///
/// Calculate type for logic unary operator like:
/// !expr:bool ==> bool
///
fn calc_type_for_unary_logic_op(
    a: &mut ast::Node,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    calc_type_for_bool_cond(a, parent, at, ctx)
}
///
/// Calculate type for arithmetic unary operator like:
/// -expr:type1 ==> type1
///
fn calc_type_for_unary_arithm_op(
    a: &mut ast::Node,
    _parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(a, ctx)?;
    let asn = get_side_node(a, ctx).expect("Side node should be calculated here");
    let atype = asn.eval_type.eval_type.clone();
    if !atype.is_numeric() && !atype.is_floating() {
        return Err(vec![comp_msg::error_numeric_type_expected(at, atype)]);
    }
    Ok(())
}
///
/// Calculate type for assign operator:
/// a:str = b:type1 ==> void
///
fn calc_type_for_assign_op(
    a: &str,
    b: &mut ast::Node,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(b, ctx)?;
    if let Some(ref mut variable) = ctx.scope_get_var(a) {
        if !variable.v_type.mutable.expect("Must be defined!") {
            return Err(vec![comp_msg::error_assign_to_immutable(at)]);
        }
        // compare variable & expression types
        let expr_side_node = get_side_node(b, ctx).expect("Side node should have been calculated");
        let expr_type = &expr_side_node.eval_type.eval_type;
        let cmp_res = compare_types(&variable.v_type.eval_type, expr_type);
        match cmp_res {
            TypeComp::Equal => {}
            TypeComp::Compliant(ref t) => {
                // we have to update either expression or variable type
                if t == expr_type {
                    // we need to update variable type
                    variable.v_type.eval_type = t.clone();
                } else {
                    set_type_all_way_down(t.clone(), b, ctx)?;
                }
            }
            TypeComp::Different => {
                return Err(vec![comp_msg::error_type_mismatch_var_assign(
                    at,
                    a,
                    &variable.v_type.eval_type,
                    expr_type,
                )]);
            }
        }
    } else {
        return Err(vec![comp_msg::error_undeclared_var(at)]);
    }
    set_type_void(parent, ctx);
    Ok(())
}
///
/// Calculate type for logic unary operator like:
/// !expr:bool ==> bool
///
fn calc_type_for_bool_cond(
    expr: &mut ast::Node,
    _parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(expr, ctx)?;
    let asn = get_side_node(expr, ctx).expect("Side node should be calculated here");
    let atype = asn.eval_type.eval_type.clone();
    if atype != EvaluatedType::Bool {
        return Err(vec![comp_msg::error_bool_type_expected(at, atype)]);
    }
    Ok(())
}
///
/// Calculate types for function call expression:
/// func(expr1, ..., exprN) -> type1
///
fn calc_type_for_fn_call(
    fn_name: &str,
    args: &mut [ast::Node],
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    // evaluate all expressions passed as arguments
    for arg in args.iter_mut() {
        eval_types(arg, ctx)?;
    }
    // do the validation
    if let Some(fn_def) = ctx.glob_functs.get(fn_name) {
        let mut errors = Vec::new();
        let mut arg_len_err: bool = false;
        if !fn_def.variadic && args.len() != fn_def.args.len() {
            arg_len_err = true;
        } else if fn_def.variadic && args.len() < fn_def.args.len() {
            arg_len_err = true;
        }
        if arg_len_err {
            let passed = args.len();
            let expected = fn_def.args.len();
            errors.push(comp_msg::error_fn_call_incorrect_no_args(
                at, passed, expected,
            ));
        }
        for (arg, arg_def) in args.iter().zip(fn_def.args.iter()) {
            let arg_type = &get_side_node(arg, ctx)
                .expect("Side node should be calculated here")
                .eval_type
                .eval_type;
            // TODO:
            let arg_type_dec = EvaluatedType::from_str(&arg_def.type_dec.typename().unwrap());
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
        let ret_type = if fn_def.ret.typename().unwrap().len() == 0 {
            Some(EvaluatedType::Void)
        } else {
            // TODO:
            EvaluatedType::from_str(&fn_def.ret.typename().unwrap())
        };
        if let Some(ret_type) = ret_type {
            set_type(
                parent,
                ctx,
                KfType {
                    mutable: Some(false),
                    eval_type: ret_type,
                },
                None,
            );
        } else {
            // TODO: not the best place for reporting return value type issues
            // TODO:
            let t = fn_def.ret.typename().unwrap().clone();
            errors.push(comp_msg::error_unrecognised_type(at, &t));
        }
        if errors.len() > 0 {
            return Err(errors);
        }
    } else {
        return Err(vec![comp_msg::error_undefined_function(at, fn_name)]);
    }
    Ok(())
}
///
/// Calculate type for variable definition:
/// let a: type1 = expr; //or
/// let a = expr; // assign expr type to a
///
fn calc_type_for_var_def(
    vardef: &mut ast::VarDef,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    // evaluate and get the expression type
    let expr_type = match &mut vardef.expr {
        Some(expr) => {
            eval_types(expr.as_mut(), ctx)?;
            get_side_node(expr, ctx)
        }
        None => None,
    };
    // is there explicit variable type?
    let mut kf_t = KfType {
        mutable: Some(vardef.mutable),
        eval_type: match &vardef.vartype {
            Some(vd) => match EvaluatedType::from_str(&vd.typename().unwrap()) {
                Some(eval_t) => {
                    // expression type should match the explicit type
                    eval_t
                }
                // it can be a struct or enum
                None => {
                    // it can be still a mismatch if e.g. expression type is int
                    EvaluatedType::ToBeInferred
                }
            },
            None => {
                // type is not defined, check if it can be inferred
                // from the expression
                EvaluatedType::ToBeInferred
            }
        },
    };
    // check if the expr type matches the explicitly defined one
    match expr_type {
        Some(expr_type) => {
            let eq = compare_types(&kf_t.eval_type, &expr_type.eval_type.eval_type);
            match eq {
                TypeComp::Equal => {
                    // great, nothing to be done
                }
                TypeComp::Compliant(t) => {
                    // one side needs to be updated/promoted
                    if kf_t.eval_type == t {
                        // update expression types
                        if let Some(expr) = &vardef.expr {
                            set_type_all_way_down(t, expr.as_ref(), ctx)?;
                        }
                    } else {
                        // update explicit variable type
                        kf_t.eval_type = t;
                    }
                }
                TypeComp::Different => {
                    // obviously an error
                    return Err(vec![comp_msg::error_type_mismatch_var_assign(
                        at,
                        &vardef.name,
                        &kf_t.eval_type,
                        &expr_type.eval_type.eval_type,
                    )]);
                }
            }
        }
        None => {
            // there is no expression, just a variable declaration
        }
    }
    ctx.scope_push_var(
        vardef.name.clone(),
        VarDefinition {
            v_type: kf_t,
            def_at: at,
        },
    );
    // whole expression evaluates to void
    set_type_void(parent, ctx);
    Ok(())
}
///
/// Calculate type for literal token
///
fn calc_type_for_literal(
    lit: &str,
    parent: &mut Option<usize>,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    let (t, v) = classify_literal(&lit);
    let mutable = if v.is_some() { Some(false) } else { None };
    // is it a variable name?
    if t == EvaluatedType::ToBeInferred && v == None {
        // check if a given variable was declared
        if let Some(variable) = ctx.scope_get_var(lit) {
            // TODO: and if so, check if the type is already determined
            set_type(
                parent,
                ctx,
                KfType {
                    mutable,
                    eval_type: variable.v_type.eval_type,
                },
                v,
            );
        } else {
            // error
            return Err(vec![comp_msg::error_undeclared_var(at)]);
        }
    } else {
        set_type(
            parent,
            ctx,
            KfType {
                mutable,
                eval_type: t,
            },
            v,
        );
    }
    Ok(())
}
/// Result of [`EvaluatedType`]s comparison
enum TypeComp {
    /// The types are equal
    Equal,
    /// One of the type is to be inferred, returned 'stronger' type
    Compliant(EvaluatedType),
    /// Completely different types
    Different,
}
/// Compares two [`EvaluatedType`] types. Additional calculation is done
/// if one of the type is to be inferred like [`EvaluatedType::ToBeInferred`],
/// [`EvaluatedType::Integer`] or [`EvaluatedType::FloatingNum`]
///    assert_eq!(compare_types(EvaluatedType::Integer, EvaluatedType::ToBeInferred) EvaluatedType::Integer);
fn compare_types(a: &EvaluatedType, b: &EvaluatedType) -> TypeComp {
    if a == b {
        return TypeComp::Equal;
    }
    if a.is_concrete() && b.is_concrete() {
        return TypeComp::Different;
    }
    // now inferred case
    for (a, b) in &[(a, b), (b, a)] {
        if a.is_concrete() {
            // b must be inferred, three options:
            match b {
                EvaluatedType::ToBeInferred => return TypeComp::Compliant((*a).clone()),
                EvaluatedType::FloatingNum => {
                    // we need to check if the concrete type is compatible with FloatingNum
                    if a.is_floating() {
                        return TypeComp::Compliant((*a).clone());
                    } else {
                        return TypeComp::Different;
                    }
                }
                EvaluatedType::Integer => {
                    if a.is_numeric() {
                        return TypeComp::Compliant((*a).clone());
                    } else {
                        return TypeComp::Different;
                    }
                }
                _ => panic!("Type not 'to be inferred' should be here: {b:?}"),
            }
        } else if *b == &EvaluatedType::ToBeInferred {
            // both are "to be inferred", the second actually might be FloatingNum or Numeric
            return TypeComp::Compliant((*a).clone());
        }
    }
    TypeComp::Different
}

fn set_type(meta_idx: &mut Option<usize>, ctx: &mut Context, t: KfType, v: Option<EvaluatedValue>) {
    match meta_idx {
        Some(idx) => {
            ctx.side_nodes[*idx].eval_type = t;
            ctx.side_nodes[*idx].eval_val = v;
        }
        None => {
            ctx.side_nodes.push(SideNode {
                eval_type: t,
                eval_val: v,
            });
            *meta_idx = Some(ctx.side_nodes.len() - 1);
        }
    }
}
fn update_type(n: &ast::Node, ctx: &mut Context, t: EvaluatedType) {
    let idx = n
        .meta_idx
        .expect("udapte_type: side node should be evaluated!");
    ctx.side_nodes[idx].eval_type.eval_type = t;
}

fn set_type_void(meta_idx: &mut Option<usize>, ctx: &mut Context) {
    let t = KfType {
        mutable: None,
        eval_type: EvaluatedType::Void,
    };
    match meta_idx {
        Some(idx) => {
            ctx.side_nodes[*idx].eval_type = t;
            ctx.side_nodes[*idx].eval_val = None;
        }
        None => {
            ctx.side_nodes.push(SideNode {
                eval_type: t,
                eval_val: None,
            });
            *meta_idx = Some(ctx.side_nodes.len() - 1);
        }
    }
}

pub fn get_side_node<'a>(n: &'a ast::Node, ctx: &'a Context) -> Option<&'a SideNode> {
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

fn set_type_all_way_down(
    t: EvaluatedType,
    n: &ast::Node,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    if !t.is_concrete() {
        // can't propagate a type that is not deduced yet
        return Ok(());
    }
    let vis = |n: &ast::Node, ctx: &mut Context| -> Result<bool, CompileMsgCol> {
        if let ast::Ntype::FnCall(_, _) = n.val {
            // we don't want to change evaluated type for e.g. function parameters
            return Ok(false);
        }
        if let ast::Ntype::Literal(ref l) = n.val {
            // that can be a variable, if so we have to update the cache
            if let Some(variable) = ctx.scope_get_var_mut(&l) {
                // TODO: what if variable has already got a concrete type?
                // println!("Setting type {t:?} to {l}");
                variable.v_type.eval_type = t.clone();
            }
        }
        update_type(n, ctx, t.clone());
        Ok(true)
    };
    walkthrough_generic(n, ctx, &vis)?;
    Ok(())
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

fn walkthrough_generic(
    n: &ast::Node,
    ctx: &mut Context,
    visitor: &impl Fn(&ast::Node, &mut Context) -> Result<bool, CompileMsgCol>,
) -> Result<(), CompileMsgCol> {
    let continue_descent = visitor(n, ctx)?;
    if !continue_descent {
        return Ok(());
    }
    match &n.val {
        ast::Ntype::Eq(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Neq(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Gt(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Ge(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Lt(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Le(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Plus(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Minus(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Slash(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Star(a, b) => {
            walkthrough_generic(a, ctx, visitor)?;
            walkthrough_generic(b, ctx, visitor)?;
        }
        ast::Ntype::Bang(a) => {
            walkthrough_generic(a, ctx, visitor)?;
        }
        ast::Ntype::UMinus(a) => {
            walkthrough_generic(a, ctx, visitor)?;
        }
        // assign
        ast::Ntype::Assign(_, b) => {
            walkthrough_generic(b, ctx, visitor)?;
        }
        // control flow
        ast::Ntype::If(if_) => {
            walkthrough_generic(&if_.cond, ctx, visitor)?;
            walkthrough_generic(&if_.body, ctx, visitor)?;
            for el in if_.elif.iter() {
                walkthrough_generic(&el.cond, ctx, visitor)?;
                walkthrough_generic(&el.body, ctx, visitor)?;
            }
            if let Some(els) = if_.else_body.as_ref() {
                walkthrough_generic(&els, ctx, visitor)?;
            }
        }
        ast::Ntype::For(for_) => {
            walkthrough_generic(&for_.in_expr, ctx, visitor)?;
            walkthrough_generic(&for_.body, ctx, visitor)?;
        }
        ast::Ntype::While(while_) => {
            walkthrough_generic(&while_.cond, ctx, visitor)?;
            walkthrough_generic(&while_.body, ctx, visitor)?;
        }
        ast::Ntype::Loop(loop_) => {
            walkthrough_generic(&loop_.body, ctx, visitor)?;
        }
        ast::Ntype::Break => {}
        ast::Ntype::Continue => {}
        // higher level structures
        ast::Ntype::Struct(_s) => {
            // TODO: s.type_id.TypeKind::Array - there might be a node to visit
            // TODO: s.members - there might be a node to visit
        }
        ast::Ntype::Enum(_) => {
            // TODO: similar case to Ntype::Struct
        }
        ast::Ntype::Impl(impl_) => {
            for n in impl_.scope.iter() {
                walkthrough_generic(n, ctx, visitor)?;
            }
        }
        ast::Ntype::Scope(a) => {
            for n in a.iter() {
                walkthrough_generic(n, ctx, visitor)?;
            }
        }
        ast::Ntype::FnDef(fndef) => {
            // TODO: args and ret might contains nodes
            walkthrough_generic(&fndef.body, ctx, visitor)?;
        }
        ast::Ntype::FnCall(_name, args) => {
            for n in args.iter() {
                walkthrough_generic(n, ctx, visitor)?;
            }
        }
        ast::Ntype::VarDef(vardef) => {
            if let Some(e) = vardef.expr.as_ref() {
                walkthrough_generic(e, ctx, visitor)?;
            }
        }
        // terminals
        ast::Ntype::String(_) => {}
        ast::Ntype::Literal(_) => {}
        ast::Ntype::Char(_) => {}
        ast::Ntype::True => {}
        ast::Ntype::False => {}
    }
    Ok(())
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
        // should compile
        let tcs = [
            "let v: i64 = 1 + 2;",
            "fn ab() {}",
            "fn a() {} fn b() { a(); }",
            "fn a() { a(); }",
            "fn a() { let b: i32 = 0; if b == 0 {}}",
            "fn a() { let mut b: i32 = 0; if true {b = 1;}}",
            "fn a() {let b = false; if b {} }",
            "fn a() {let b = 1; let c: i8 = 0; if b == c {} }",
            "fn a() {let b = 1; let c: i32 = 0; if b * c == 0 {} }",
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
        // shouldn't compile
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
