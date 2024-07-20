// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::ast;
use crate::lang_def::ParsingError;
use crate::lang_def::TextPoint;
use crate::type_system::*;
use std::collections::HashMap;

pub struct Context {
    pub side_nodes: Vec<SideNode>,
    pub glob_functs: HashMap<String, (Vec<ast::VarDecl>, ast::TypeDecl)>,
}

pub struct InterRepr {
    pub syntree: ast::Node,
    pub ctx: Context,
}

impl InterRepr {
    pub fn from(mut syntree: ast::Node) -> Result<Self, Vec<ParsingError>> {
        let mut ctx = Context {
            side_nodes: Vec::new(),
            glob_functs: HashMap::new(),
        };
        first_pass(&mut syntree, &mut ctx)?;
        second_pass(&mut syntree, &mut ctx)?;
        Ok(Self { syntree, ctx })
    }
}

fn first_pass(syntree: &mut ast::Node, ctx: &mut Context) -> Result<(), Vec<ParsingError>> {
    // collect all function definitions
    collect_glob_functions(syntree, ctx)?;
    // evaluate expression types
    eval_types(syntree, ctx)?;
    Ok(())
}

fn second_pass(syntree: &mut ast::Node, ctx: &mut Context) -> Result<(), Vec<ParsingError>> {
    Ok(())
}

fn collect_glob_functions(
    syntree: &mut ast::Node,
    ctx: &mut Context,
) -> Result<(), Vec<ParsingError>> {
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
                ret,
                body: _,
            } = fndef;
            // check if the name is correct
            if let Some(c) = name.chars().next() {
                if !c.is_ascii_alphabetic() {
                    errors.push(ParsingError {
                        msg: "function name should start with alphabetic ascii character".into(),
                        details: format!("first character >{c}< is not allowed in function name"),
                        at: syntree.at,
                    });
                }
            }
            if !ctx
                .glob_functs
                .insert(name.clone(), (args.clone(), ret.clone()))
                .is_none()
            {
                // collision - this function already defined
                errors.push(ParsingError {
                    msg: "function with this name already defined".into(),
                    details: "function name should be unique across module scope".into(),
                    at: syntree.at,
                })
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

fn eval_types(n: &mut ast::Node, s: &mut Context) -> Result<(), Vec<ParsingError>> {
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
                let err = ParsingError {
                    msg: "bang operator can be applied only to a boolean type".into(),
                    details: format!("righside expression evaluates to {:?}", atype),
                    at: n.at,
                };
                return Err(vec![err]);
            }
        }
        ast::Ntype::UMinus(a) => {
            eval_types(a.as_mut(), s)?;
            let asn = get_side_node(a, s).expect("Side node should be calculated here");
            let atype = asn.eval_type.eval_type.clone();
            if !atype.is_numeric() && !atype.is_floating() {
                let err = ParsingError {
                    msg: "unary minus operator can be applied either to numeric of floating types"
                        .into(),
                    details: format!("righside expression evaluates to {:?}", atype),
                    at: n.at,
                };
                return Err(vec![err]);
            }
        } // unary minus
        // assign
        ast::Ntype::Assign(a, b) => {
            // TODO: a is a literal, lookup type and check if rhs == lhs
            eval_types(b.as_mut(), s)?;
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
                let err = ParsingError {
                    msg: "if expression should evaluate to boolean type".into(),
                    details: format!("if expression evaluates to {:?}", atype),
                    at: n.at,
                };
                errors.push(err);
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
                    let err = ParsingError {
                        msg: "else if expression should evaluate to boolean type".into(),
                        details: format!("if expression evaluates to {:?}", atype),
                        at: n.at,
                    };
                    errors.push(err);
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
                let err = ParsingError {
                    msg: "while expression should evaluate to boolean type".into(),
                    details: format!("while expression evaluates to {:?}", atype),
                    at: n.at,
                };
                errors.push(err);
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
        ast::Ntype::Scope(a) => {
            for n in a {
                eval_types(n, s)?;
            }

            set_type_void(n, s);
        }
        ast::Ntype::FnDef(fndef) => {
            eval_types(fndef.body.as_mut(), s)?;
            set_type_void(n, s);
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
                    errors.push(ParsingError {
                        msg: "incorrect arguments passed".into(),
                        details: format!("passed {passed} arguments, expected: {expected}"),
                        at: n.at,
                    });
                }
                for (arg, arg_def) in args.iter().zip(args_def) {
                    let arg_type = &get_side_node(arg, s)
                        .expect("Side node should be calculated here")
                        .eval_type
                        .eval_type;
                    let arg_type_dec = EvaluatedType::from_str(&arg_def.type_dec.typename);
                    if let Some(arg_type_dec) = arg_type_dec {
                        if *arg_type != arg_type_dec {
                            // error
                            errors.push(ParsingError {
                                msg: "argument type mismatch".into(),
                                details: format!("expected {arg_type_dec:?} but the expression evaluates to {arg_type:?}"),
                                at: arg.at,
                            });
                        }
                    } else {
                        // TODO: not the best place for reporting this error
                        let t = arg_def.type_dec.typename.clone();
                        errors.push(ParsingError {
                            msg: "type not recognised".into(),
                            details: format!("type >{t}< is not defined"),
                            at: arg.at,
                        });
                    }
                }
                // set return type
                let ret_type = if ret_def.typename.len() == 0 {
                    Some(EvaluatedType::Void)
                } else {
                    EvaluatedType::from_str(&ret_def.typename)
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
                    // TODO: not the best place for reporting this error
                    let t = ret_def.typename.clone();
                    errors.push(ParsingError {
                        msg: "function return value type not recognised".into(),
                        details: format!("unknown type >{t}<"),
                        at: n.at,
                    });
                }
                if errors.len() > 0 {
                    return Err(errors);
                }
            } else {
                return Err(vec![ParsingError {
                    msg: "function is not defined".into(),
                    details: format!(
                        "function >{name}< is not defined, check if you've made a typo"
                    ),
                    at: n.at,
                }]);
            }
        }
        ast::Ntype::VarDef(vardef) => {
            match &mut vardef.expr {
                Some(expr) => {
                    eval_types(expr.as_mut(), s)?;
                }
                None => {}
            }
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
) -> Result<EvaluatedType, Vec<ParsingError>> {
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
        // we need to emit a compiler error
        let err = ParsingError {
            msg: "leftside expression and righside expression must evaluate to the same type"
                .into(),
            details: format!(
                "leftside expression evaluates to {:?} while righside expression evaluates to {:?}",
                asn.eval_type.eval_type, bsn.eval_type.eval_type
            ),
            at,
        };
        return Err(vec![err]);
    }
    Ok(atype)
}
fn validate_type_for_arithm_op(
    a: &mut ast::Node,
    b: &mut ast::Node,
    at: TextPoint,
    ctx: &mut Context,
) -> Result<EvaluatedType, Vec<ParsingError>> {
    eval_types(a, ctx)?;
    eval_types(b, ctx)?;
    let t = determine_type_for_a_b(&a, &b, at, ctx)?;
    if !t.is_numeric() && !t.is_floating() {
        let err = ParsingError {
            msg: "operator can be applied to either numeric or floating numbers".into(),
            details: format!("leftside and rightside expressions evaluate to {:?}", t),
            at,
        };
        return Err(vec![err]);
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
    use crate::lang_def::Lang;
    use crate::lexer;
    use crate::parser;

    fn throw_errors(errs: Vec<ParsingError>) -> Result<(), String> {
        if errs.len() == 0 {
            Ok(())
        } else {
            let mut error = String::new();
            for e in errs {
                let msg = format!("Syntax error at {}, {}: {}", e.at.line, e.at.col, e.msg);
                error.push_str(&msg);
                error += "\n";
            }
            Err(error)
        }
    }

    fn parse_code(code: &str) -> Result<ast::Node, String> {
        let (tokens, errors) = lexer::tokenize(&Lang::new(), code);
        throw_errors(errors)?;
        let ast_tree = parser::parse(&tokens);
        match ast_tree {
            Ok(ast) => Ok(ast),
            Err(errs) => {
                let mut err_msg = String::new();
                for e in errs {
                    err_msg += code;
                    err_msg += &format!("\n{}^\n", " ".repeat(e.at.col - 1));
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
        ];
        for c in tcs {
            let ast = parse_code(c).unwrap();
            let inter = InterRepr::from(ast);
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
        ];
        for c in tcs {
            let ast = parse_code(c).unwrap();
            let inter = InterRepr::from(ast);
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
