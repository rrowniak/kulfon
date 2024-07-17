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

pub struct Context {
    pub side_nodes: Vec<SideNode>,
}

pub struct InterRepr {
    pub syntree: ast::Node,
    pub ctx: Context,
}

impl InterRepr {
    pub fn from(mut syntree: ast::Node) -> Result<Self, Vec<ParsingError>> {
        let mut ctx = Context {
            side_nodes: Vec::new(),
        };
        first_pass(&mut syntree, &mut ctx)?;
        second_pass(&mut syntree, &mut ctx)?;
        Ok(Self { syntree, ctx })
    }
}

fn first_pass(syntree: &mut ast::Node, ctx: &mut Context) -> Result<(), Vec<ParsingError>> {
    // evaluate expression types
    eval_types(syntree, ctx)?;
    Ok(())
}

fn second_pass(syntree: &mut ast::Node, ctx: &mut Context) -> Result<(), Vec<ParsingError>> {
    Ok(())
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
            let val = calculate_operator!(
                get_side_node(&a, s).unwrap().eval_val.clone().unwrap(),
                ==,
                get_side_node(&b, s).unwrap().eval_val.clone().unwrap()
            );
            let val = if let Some(v) = val {
                Some(EvaluatedValue::Bool(v))
            } else {
                None
            };
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
        ast::Ntype::Neq(a, b) => {}
        ast::Ntype::Gt(a, b) => {}
        ast::Ntype::Ge(a, b) => {}
        ast::Ntype::Lt(a, b) => {}
        ast::Ntype::Le(a, b) => {}
        ast::Ntype::Plus(a, b) => {}
        ast::Ntype::Minus(a, b) => {}
        ast::Ntype::Slash(a, b) => {}
        ast::Ntype::Star(a, b) => {}
        ast::Ntype::Bang(a) => {}
        ast::Ntype::UMinus(a) => {} // unary minu
        // assign
        ast::Ntype::Assign(a, b) => {
            eval_types(b.as_mut(), s)?;
        }
        // control flow
        ast::Ntype::If(if_) => {}
        ast::Ntype::For(for_) => {}
        ast::Ntype::While(while_) => {}
        ast::Ntype::Loop(loop_) => {}
        ast::Ntype::Break => set_type(n, s, KfType::from_literal(EvaluatedType::Never), None),
        ast::Ntype::Continue => set_type(n, s, KfType::from_literal(EvaluatedType::Never), None),
        // higher level structures
        ast::Ntype::Scope(a) => {
            for n in a {
                eval_types(n, s)?;
            }
        }
        ast::Ntype::FnDef(fndef) => {
            eval_types(fndef.body.as_mut(), s)?;
        }
        ast::Ntype::FnCall(name, args) => {}
        ast::Ntype::VarDef(vardef) => match &mut vardef.expr {
            Some(expr) => {
                eval_types(expr.as_mut(), s)?;
            }
            None => {}
        },
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

    println!("asn={:?}\nbsn={:?}", asn, bsn);

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
        let tcs = ["let v = 1 + 2;"];
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
        let tcs = [r#"fn a(){1 == "s"; }"#, "let v = 1 == \"srt\";"];
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
