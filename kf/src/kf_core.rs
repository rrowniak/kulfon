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

#[derive(Copy, Clone, Debug)]
pub struct SideNodeRef(u32);

impl SideNodeRef {
    pub fn from_node_ref(nref: ast::NodeRef) -> Self {
        Self(nref.as_usize() as u32)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SideNode {
    pub eval_type: KfType,
    pub eval_val: Option<EvaluatedValue>,
}

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
    side_node_ref: SideNodeRef,
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
    pub fn get_side_node(&self, i: SideNodeRef) -> &SideNode {
        &self.side_nodes[i.0 as usize]
    }
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

    fn update_var_type(&mut self, name: &str, t: EvaluatedType) -> Option<EvaluatedType> {
        // println!("Updating variable {name} to {t:?}");
        let (t, indx) = if let Some(v) = self.scope_get_var_mut(name) {
            (
                std::mem::replace(&mut v.v_type.eval_type, t),
                v.side_node_ref,
            )
        } else {
            return None;
        };
        self.side_nodes[indx.0 as usize].eval_type.eval_type = t.clone();
        Some(t)
    }
}

pub struct InterRepr {
    pub syntree: ast::Tree,
    pub ctx: Context,
}

impl InterRepr {
    pub fn from(syntree: ast::Tree, builtin: ast::Tree) -> Result<Self, CompileMsgCol> {
        let mut ctx = Context {
            side_nodes: Vec::with_capacity(syntree.flat.len()),
            glob_functs: HashMap::new(),
            // add a global scope
            temp_scope: Vec::new(),
        };
        ctx.side_nodes
            .resize(syntree.flat.len(), SideNode::default());
        // collect all function definitions
        collect_glob_functions(&builtin, builtin.root, &mut ctx)?;
        collect_glob_functions(&syntree, syntree.root, &mut ctx)?;
        // evaluate expression types
        eval_types(&syntree, syntree.root, &mut ctx)?;
        Ok(Self { syntree, ctx })
    }
}

fn collect_glob_functions(
    syntree: &ast::Tree,
    nref: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    let mut errors = Vec::new();
    match &syntree.get(nref).val {
        ast::Ntype::Scope(scope) => {
            for st in scope.iter() {
                collect_glob_functions(syntree, *st, ctx)?;
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
                    errors.push(comp_msg::error_inv_var_fn_name(syntree.get(nref).at));
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
                errors.push(comp_msg::error_fn_name_in_use(syntree.get(nref).at));
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

fn eval_types(
    tree: &ast::Tree,
    nref: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    let n = tree.get(nref);
    match n.val {
        // operators
        // binary operators:
        // * calculate a and b
        // * evaluate expression type based on a and b
        // * For logic operators resulting type is bool
        // * Try to calculate the value if possible
        ast::Ntype::Eq(a, b) => {
            calc_type_for_bin_nontransient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Neq(a, b) => {
            calc_type_for_bin_nontransient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Gt(a, b) => {
            calc_type_for_bin_arithmetic_nontransient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Ge(a, b) => {
            calc_type_for_bin_arithmetic_nontransient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Lt(a, b) => {
            calc_type_for_bin_arithmetic_nontransient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Le(a, b) => {
            calc_type_for_bin_arithmetic_nontransient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Plus(a, b) => {
            calc_type_for_bin_arithmetic_transient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Minus(a, b) => {
            calc_type_for_bin_arithmetic_transient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Slash(a, b) => {
            calc_type_for_bin_arithmetic_transient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Star(a, b) => {
            calc_type_for_bin_arithmetic_transient_op(tree, nref, a, b, ctx)?;
        }
        ast::Ntype::Bang(a) => {
            calc_type_for_unary_logic_op(tree, nref, a, ctx)?;
        }
        ast::Ntype::UMinus(a) => {
            // unary minus
            calc_type_for_unary_arithm_op(tree, nref, a, ctx)?;
        }
        // assign
        ast::Ntype::Assign(ref a, b) => {
            calc_type_for_assign_op(tree, nref, a, b, ctx)?;
        }
        // control flow
        ast::Ntype::If(ref if_) => {
            // condition must be bool
            calc_type_for_bool_cond(tree, nref, if_.cond, ctx)?;
            // evaluate body
            eval_types(tree, if_.body, ctx)?;
            // evaluate elif
            for elif in if_.elif.iter() {
                calc_type_for_bool_cond(tree, nref, elif.cond, ctx)?;
                // body
                eval_types(tree, elif.body, ctx)?;
            }
            // evaluate else
            if let Some(else_body) = &if_.else_body {
                eval_types(tree, *else_body, ctx)?;
            }
            set_type_void(nref, ctx);
        }
        ast::Ntype::For(ref for_) => {
            // TODO: handle matching patterns
            eval_types(tree, for_.in_expr, ctx)?;
            eval_types(tree, for_.body, ctx)?;
            set_type_void(nref, ctx);
        }
        ast::Ntype::While(ref while_) => {
            calc_type_for_bool_cond(tree, nref, while_.cond, ctx)?;
            // evaluate body
            eval_types(tree, while_.body, ctx)?;
            set_type_void(nref, ctx);
        }
        ast::Ntype::Loop(ref loop_) => {
            eval_types(tree, loop_.body, ctx)?;
            set_type_void(nref, ctx);
        }
        ast::Ntype::Break => set_type(nref, ctx, KfType::from_literal(EvaluatedType::Never), None),
        ast::Ntype::Continue => {
            set_type(nref, ctx, KfType::from_literal(EvaluatedType::Never), None)
        }
        // higher level structures
        ast::Ntype::Struct(_) => unimplemented!(),
        ast::Ntype::Enum(_) => unimplemented!(),
        ast::Ntype::Impl(_) => unimplemented!(),
        ast::Ntype::Scope(ref a) => {
            ctx.scope_push(true);
            for node in a.iter() {
                eval_types(tree, *node, ctx)?;
            }
            set_type_void(nref, ctx);
            let not_deduced_err = ctx
                .scope_pop()
                .iter()
                .map(|(n, t)| comp_msg::error_var_type_not_deduced(t.def_at, n))
                .collect::<CompileMsgCol>();
            if not_deduced_err.len() > 0 {
                return Err(not_deduced_err);
            }
        }
        ast::Ntype::FnDef(ref fndef) => {
            ctx.scope_push(false);
            eval_types(tree, fndef.body, ctx)?;
            set_type_void(nref, ctx);
            let not_deduced_err = ctx
                .scope_pop()
                .iter()
                .map(|(n, t)| comp_msg::error_var_type_not_deduced(t.def_at, n))
                .collect::<CompileMsgCol>();
            if not_deduced_err.len() > 0 {
                return Err(not_deduced_err);
            }
        }
        ast::Ntype::FnCall(ref name, ref args) => {
            calc_type_for_fn_call(tree, nref, &name, args, ctx)?;
        }
        ast::Ntype::VarDef(ref vardef) => {
            calc_type_for_var_def(tree, nref, vardef, ctx)?;
        }
        // terminals
        ast::Ntype::String(ref ss) => {
            let ss = ss.clone();
            set_type(
                nref,
                ctx,
                KfType::from_literal(EvaluatedType::String),
                Some(EvaluatedValue::String(ss)),
            );
        }
        ast::Ntype::Literal(ref lit) => {
            calc_type_for_literal(tree, nref, &lit, ctx)?;
        }
        ast::Ntype::Char(ref c) => {
            let c = c.clone();
            set_type(
                nref,
                ctx,
                KfType::from_literal(EvaluatedType::Char),
                Some(EvaluatedValue::Char(
                    c.chars().next().expect("Can't extract character"),
                )),
            );
        }
        ast::Ntype::True => set_type(
            nref,
            ctx,
            KfType::from_literal(EvaluatedType::Bool),
            Some(EvaluatedValue::Bool(false)),
        ),
        ast::Ntype::False => set_type(
            nref,
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
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: ast::NodeRef,
    b: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(tree, a, ctx)?;
    eval_types(tree, b, ctx)?;
    let t = determine_type_for_a_b(tree, parent, a, b, ctx)?;
    // println!("type ab = {t:?}");
    set_type_all_way_down(tree, a, ctx, &t)?;
    set_type_all_way_down(tree, b, ctx, &t)?;
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
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: ast::NodeRef,
    b: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(tree, a, ctx)?;
    eval_types(tree, b, ctx)?;
    let t = determine_type_for_a_b(tree, parent, a, b, ctx)?;
    if !t.is_numeric() && !t.is_floating() {
        return Err(vec![comp_msg::error_numeric_type_for_bin_op(
            tree.get(parent).at,
            t.clone(),
            t,
        )]);
    }

    set_type_all_way_down(tree, a, ctx, &t)?;
    set_type_all_way_down(tree, b, ctx, &t)?;
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
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: ast::NodeRef,
    b: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(tree, a, ctx)?;
    eval_types(tree, b, ctx)?;
    let t = determine_type_for_a_b(tree, parent, a, b, ctx)?;
    if !t.is_numeric() && !t.is_floating() {
        return Err(vec![comp_msg::error_numeric_type_for_bin_op(
            tree.get(parent).at,
            t.clone(),
            t,
        )]);
    }
    set_type_all_way_down(tree, a, ctx, &t)?;
    set_type_all_way_down(tree, b, ctx, &t)?;
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
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    calc_type_for_bool_cond(tree, parent, a, ctx)
}
///
/// Calculate type for arithmetic unary operator like:
/// -expr:type1 ==> type1
///
fn calc_type_for_unary_arithm_op(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(tree, a, ctx)?;
    let asn = get_side_node(a, ctx);
    let atype = asn.eval_type.eval_type.clone();
    if !atype.is_numeric() && !atype.is_floating() {
        return Err(vec![comp_msg::error_numeric_type_expected(
            tree.get(parent).at,
            atype,
        )]);
    }
    Ok(())
}
///
/// Calculate type for assign operator:
/// a:str = b:type1 ==> void
///
fn calc_type_for_assign_op(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: &str,
    b: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(tree, b, ctx)?;
    if let Some(variable) = ctx.scope_get_var(a) {
        if !variable.v_type.mutable.expect("Must be defined!") {
            return Err(vec![comp_msg::error_assign_to_immutable(
                tree.get(parent).at,
            )]);
        }
        // compare variable & expression types
        let expr_side_node = get_side_node(b, ctx);
        let expr_type = &expr_side_node.eval_type.eval_type;
        let cmp_res = compare_types(&variable.v_type.eval_type, expr_type);
        match cmp_res {
            TypeComp::Equal => {}
            TypeComp::Compliant(ref t) => {
                // we have to update either expression or variable type
                if t == expr_type {
                    // we need to update variable type
                    ctx.update_var_type(a, t.clone());
                } else {
                    set_type_all_way_down(tree, b, ctx, t)?;
                }
            }
            TypeComp::Different => {
                return Err(vec![comp_msg::error_type_mismatch_var_assign(
                    tree.get(parent).at,
                    a,
                    &variable.v_type.eval_type,
                    expr_type,
                )]);
            }
        }
    } else {
        return Err(vec![comp_msg::error_undeclared_var(tree.get(parent).at)]);
    }
    set_type_void(parent, ctx);
    Ok(())
}
///
/// Calculate type for logic unary operator like:
/// !expr:bool ==> bool
///
fn calc_type_for_bool_cond(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    expr: ast::NodeRef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    eval_types(tree, expr, ctx)?;
    let asn = get_side_node(expr, ctx);
    let atype = asn.eval_type.eval_type.clone();
    if atype != EvaluatedType::Bool {
        return Err(vec![comp_msg::error_bool_type_expected(
            tree.get(parent).at,
            atype,
        )]);
    }
    Ok(())
}
///
/// Calculate types for function call expression:
/// func(expr1, ..., exprN) -> type1
///
fn calc_type_for_fn_call(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    fn_name: &str,
    args: &[ast::NodeRef],
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    // evaluate all expressions passed as arguments
    for arg in args.iter() {
        eval_types(tree, *arg, ctx)?;
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
                tree.get(parent).at,
                passed,
                expected,
            ));
        }
        for (arg, arg_def) in args.iter().zip(fn_def.args.iter()) {
            let arg_type = get_side_node(*arg, ctx).eval_type.eval_type.clone();
            // TODO:
            let arg_type_dec = EvaluatedType::from_str(&arg_def.type_dec.typename().unwrap());
            if let Some(arg_type_dec) = arg_type_dec {
                if arg_type != arg_type_dec {
                    errors.push(comp_msg::error_fn_call_arg_mismatch(
                        tree.get(*arg).at,
                        arg_type.clone(),
                        arg_type_dec,
                    ));
                }
            } else {
                // TODO: not the best place for reporting this error
                // TODO:
                let t = arg_def.type_dec.typename().unwrap().clone();
                errors.push(comp_msg::error_unrecognised_type(tree.get(*arg).at, &t));
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
            errors.push(comp_msg::error_unrecognised_type(tree.get(parent).at, &t));
        }
        if errors.len() > 0 {
            return Err(errors);
        }
    } else {
        return Err(vec![comp_msg::error_undefined_function(
            tree.get(parent).at,
            fn_name,
        )]);
    }
    Ok(())
}
///
/// Calculate type for variable definition:
/// let a: type1 = expr; //or
/// let a = expr; // assign expr type to a
///
fn calc_type_for_var_def(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    vardef: &ast::VarDef,
    ctx: &mut Context,
) -> Result<(), CompileMsgCol> {
    // evaluate and get the expression type
    let expr_type = match vardef.expr {
        Some(expr) => {
            eval_types(tree, expr, ctx)?;
            Some(get_side_node(expr, ctx))
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
                            set_type_all_way_down(tree, *expr, ctx, &t)?;
                        }
                    } else {
                        // update explicit variable type
                        kf_t.eval_type = t;
                    }
                }
                TypeComp::Different => {
                    // obviously an error
                    return Err(vec![comp_msg::error_type_mismatch_var_assign(
                        tree.get(parent).at,
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
    // whole expression evaluates to void
    set_type_void(parent, ctx);
    ctx.scope_push_var(
        vardef.name.clone(),
        VarDefinition {
            v_type: kf_t,
            def_at: tree.get(parent).at,
            side_node_ref: SideNodeRef(parent.as_usize() as u32),
        },
    );
    Ok(())
}
///
/// Calculate type for literal token
///
fn calc_type_for_literal(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    lit: &str,
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
            return Err(vec![comp_msg::error_undeclared_var(tree.get(parent).at)]);
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

fn set_type(node: ast::NodeRef, ctx: &mut Context, t: KfType, v: Option<EvaluatedValue>) {
    ctx.side_nodes[node.as_usize()].eval_type = t;
    ctx.side_nodes[node.as_usize()].eval_val = v;
}
fn update_type(n: ast::NodeRef, ctx: &mut Context, t: EvaluatedType) {
    ctx.side_nodes[n.as_usize()].eval_type.eval_type = t;
}

fn set_type_void(node: ast::NodeRef, ctx: &mut Context) {
    let t = KfType {
        mutable: None,
        eval_type: EvaluatedType::Void,
    };
    ctx.side_nodes[node.as_usize()].eval_type = t;
    ctx.side_nodes[node.as_usize()].eval_val = None;
}

pub fn get_side_node<'a>(n: ast::NodeRef, ctx: &'a Context) -> &'a SideNode {
    &ctx.side_nodes[n.as_usize()]
}

fn determine_type_for_a_b(
    tree: &ast::Tree,
    parent: ast::NodeRef,
    a: ast::NodeRef,
    b: ast::NodeRef,
    ctx: &Context,
) -> Result<EvaluatedType, CompileMsgCol> {
    let asn = get_side_node(a, ctx);
    let bsn = get_side_node(b, ctx);

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
            tree.get(parent).at,
            atype,
            btype,
        )]);
    }
    Ok(atype)
}

fn set_type_all_way_down(
    tree: &ast::Tree,
    n: ast::NodeRef,
    ctx: &mut Context,
    t: &EvaluatedType,
) -> Result<(), CompileMsgCol> {
    if !t.is_concrete() {
        // can't propagate a type that is not deduced yet
        return Ok(());
    }
    let vis =
        |tree: &ast::Tree, node: ast::NodeRef, ctx: &mut Context| -> Result<bool, CompileMsgCol> {
            let n = tree.get(node);
            if let ast::Ntype::FnCall(_, _) = n.val {
                // we don't want to change evaluated type for e.g. function parameters
                return Ok(false);
            }
            if let ast::Ntype::Literal(ref l) = n.val {
                // that can be a variable, if so we have to update the cache
                ctx.update_var_type(&l, t.clone());
            }
            update_type(node, ctx, t.clone());
            Ok(true)
        };
    walkthrough_generic(tree, n, ctx, &vis)?;
    Ok(())
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
    tree: &ast::Tree,
    nref: ast::NodeRef,
    ctx: &mut Context,
    visitor: &impl Fn(&ast::Tree, ast::NodeRef, &mut Context) -> Result<bool, CompileMsgCol>,
) -> Result<(), CompileMsgCol> {
    let continue_descent = visitor(tree, nref, ctx)?;
    if !continue_descent {
        return Ok(());
    }
    match &tree.get(nref).val {
        ast::Ntype::Eq(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Neq(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Gt(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Ge(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Lt(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Le(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Plus(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Minus(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Slash(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Star(a, b) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        ast::Ntype::Bang(a) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
        }
        ast::Ntype::UMinus(a) => {
            walkthrough_generic(tree, *a, ctx, visitor)?;
        }
        // assign
        ast::Ntype::Assign(_, b) => {
            walkthrough_generic(tree, *b, ctx, visitor)?;
        }
        // control flow
        ast::Ntype::If(if_) => {
            walkthrough_generic(tree, if_.cond, ctx, visitor)?;
            walkthrough_generic(tree, if_.body, ctx, visitor)?;
            for el in if_.elif.iter() {
                walkthrough_generic(tree, el.cond, ctx, visitor)?;
                walkthrough_generic(tree, el.body, ctx, visitor)?;
            }
            if let Some(els) = if_.else_body {
                walkthrough_generic(tree, els, ctx, visitor)?;
            }
        }
        ast::Ntype::For(for_) => {
            walkthrough_generic(tree, for_.in_expr, ctx, visitor)?;
            walkthrough_generic(tree, for_.body, ctx, visitor)?;
        }
        ast::Ntype::While(while_) => {
            walkthrough_generic(tree, while_.cond, ctx, visitor)?;
            walkthrough_generic(tree, while_.body, ctx, visitor)?;
        }
        ast::Ntype::Loop(loop_) => {
            walkthrough_generic(tree, loop_.body, ctx, visitor)?;
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
            for node in impl_.scope.iter() {
                walkthrough_generic(tree, *node, ctx, visitor)?;
            }
        }
        ast::Ntype::Scope(a) => {
            for node in a.iter() {
                walkthrough_generic(tree, *node, ctx, visitor)?;
            }
        }
        ast::Ntype::FnDef(fndef) => {
            // TODO: args and ret might contains nodes
            walkthrough_generic(tree, fndef.body, ctx, visitor)?;
        }
        ast::Ntype::FnCall(_name, args) => {
            for node in args.iter() {
                walkthrough_generic(tree, *node, ctx, visitor)?;
            }
        }
        ast::Ntype::VarDef(vardef) => {
            if let Some(e) = vardef.expr {
                walkthrough_generic(tree, e, ctx, visitor)?;
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

    fn parse_code(code: &str) -> Result<(ast::Tree, ast::Tree), String> {
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
