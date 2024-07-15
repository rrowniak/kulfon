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

pub struct InterRepr {
    pub syntree: ast::Node,
    pub side_nodes: Vec<SideNode>,
}

impl InterRepr {
    pub fn from(syntree: ast::Node) -> Self {
        Self {
            syntree,
            side_nodes: Vec::new(),
        }
    }

    pub fn compile(&mut self) -> Result<(), Vec<ParsingError>> {
        self.first_pass()?;
        self.second_pass()?;
        Ok(())
    }

    fn first_pass(&mut self) -> Result<(), Vec<ParsingError>> {
        // evaluate expression types
        eval_types(&mut self.syntree, &mut self.side_nodes)?;
        Ok(())
    }

    fn second_pass(&mut self) -> Result<(), Vec<ParsingError>> {
        Ok(())
    }
}

fn eval_types(n: &mut ast::Node, s: &mut [SideNode]) -> Result<(), Vec<ParsingError>> {
    match &mut n.val {
        // operators
        // binary operators:
        // * calculate a and b
        // * a and b have to be the same type
        // * For logic operators resulting type is bool
        // * Try to calculate the value if possible
        ast::Ntype::Eq(a, b) => {
            eval_types(a.as_mut(), s)?;
            eval_types(b.as_mut(), s)?;
            types_must_be_equal(&a, &b, n.at, s)?;
            let val = evaluate_logic_op(&a, &b, |a, b| a == b);
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
        ast::Ntype::Assign(a, b) => {}
        // control flow
        ast::Ntype::If(if_) => {}
        ast::Ntype::For(for_) => {}
        ast::Ntype::While(while_) => {}
        ast::Ntype::Loop(loop_) => {}
        ast::Ntype::Break => set_type(n, s, KfType::from_literal(EvaluatedType::Never), None),
        ast::Ntype::Continue => set_type(n, s, KfType::from_literal(EvaluatedType::Never), None),
        // higher level structures
        ast::Ntype::Scope(a) => {}
        ast::Ntype::FnDef(fndef) => {}
        ast::Ntype::FnCall(name, args) => {}
        ast::Ntype::VarDef(vardef) => {}
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

fn set_type(n: &mut ast::Node, side_nodes: &mut [SideNode], t: KfType, v: Option<EvaluatedValue>) {
    match n.meta_idx {
        Some(idx) => {
            side_nodes[idx].eval_type = t;
        }
        None => panic!("That should not happes as we already initialized side nodes!"),
    }
}

fn get_side_node<'a>(n: &'a ast::Node, side_nodes: &'a [SideNode]) -> Option<&'a SideNode> {
    if let Some(side_node) = n.meta_idx {
        Some(&side_nodes[side_node])
    } else {
        None
    }
}

fn types_must_be_equal(
    a: &ast::Node,
    b: &ast::Node,
    at: TextPoint,
    side_nodes: &[SideNode],
) -> Result<(), Vec<ParsingError>> {
    let asn = get_side_node(a, side_nodes).expect("Side node should be calculated here!");
    let bsn = get_side_node(b, side_nodes).expect("Side node should be calculated here (2)");

    // TODO: unfortunate names - figure out better namings
    if asn.eval_type.eval_type == EvaluatedType::ToBeInferred
        || bsn.eval_type.eval_type == EvaluatedType::ToBeInferred
    {
        // TODO: This is a place where we can suggest which type might be expected
        // Now just say "ok" as this will be resolved at 2nd pass
        return Ok(());
    }

    if asn.eval_type.eval_type != bsn.eval_type.eval_type {
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
    Ok(())
}

fn evaluate_logic_op<F>(a: &ast::Node, b: &ast::Node, op: F) -> Option<EvaluatedValue>
where
    F: Fn(bool, bool) -> bool,
{
    return Some(EvaluatedValue::Bool(op(true, true)));
}

struct SideNode {
    eval_type: KfType,
    eval_val: Option<EvaluatedValue>,
}

impl SideNode {
    fn new() -> Self {
        Self {
            eval_type: KfType {
                mutable: None,
                eval_type: EvaluatedType::ToBeInferred,
            },
            eval_val: None,
        }
    }
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
