// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 11.06.2024
// ---------------------------------------------------

/*
************** Backus–Naur form grammar parser **************
* Rules:
* - non-terminals must be enclosed in brackets <nonterminal>
* - terminals must be surrounded in "quotes"
* - comments: a valid comment start with '//' and ends with a new line

To understand the recursive descend parser, a simplified BNF grammar
for BNF parser is presented below:

<bnf> ::= <nterm_s> "::=" <rhs>

<rhs> ::= <symbol>+ ( "|" <symbol> )* <rhs> | ";"
<symbol> ::= <term> | <nterm> | <group>

<term> ::= <term_s> <cnt>
<term_s> ::= "\"" [a-z] "\""
<nterm> ::= <nterm_s> <cnt>
<nterm_s> ::= "<" [a-z] ">"

<group> ::= <group_s> <cnt>
<group_s> ::= "(" <rhs> ")"

<cnt> ::= "?" | "*" | "+" | E
*/

use crate::lang_def;
use crate::lexer;
use crate::parse_iter::ParseIter;
use core::fmt;
use std::boxed::Box;

const TRACE_BNF: bool = true;

macro_rules! trace {
    // This pattern matches when no arguments are provided
    () => {
        if TRACE_BNF {
            println!();
        }
    };
    // This pattern matches when there are one or more arguments
    ($($arg:tt)*) => {
        if TRACE_BNF {
            println!("[TRACE] [{}:{}]: {}", file!(), line!(), format!($($arg)*));
        }
    };
}

type Iter<'a> = ParseIter<'a, lexer::Token>;

#[derive(PartialEq, Debug)]
pub enum Symbol {
    Terminal(String),
    NonTerminal(String),
    Group(Vec<Symbol>),
    Or(Vec<Symbol>),
    ZeroOrMore(Box<Symbol>),
    AtLeastOnce(Box<Symbol>),
    ZeroOrOne(Box<Symbol>),
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Symbol::Terminal(t) => write!(f, "term:{}", t),
            Symbol::NonTerminal(nt) => write!(f, "nonterm:{}", nt),
            Symbol::Group(g) => {
                write!(f, "group[")?;
                for s in g {
                    write!(f, "{},", s)?;
                }
                write!(f, "]")
            }
            Symbol::Or(g) => {
                write!(f, "or[")?;
                for s in g {
                    write!(f, "{},", s)?;
                }
                write!(f, "]")
            }
            Symbol::ZeroOrOne(s) => write!(f, "{}+", s),
            Symbol::AtLeastOnce(s) => write!(f, "{}+", s),
            Symbol::ZeroOrMore(s) => write!(f, "{}*", s),
        }
    }
}

pub struct Rule {
    pub non_terminal: Symbol,
    pub symbols: Vec<Symbol>,
}

pub struct BNF {
    pub rules: Vec<Rule>,
}

pub fn parse(grammar: &str) -> Result<BNF, String> {
    // define language
    let mut lang = lang_def::Lang::new_empty();
    lang.range_based
        .push(lang_def::RangeBased::LineComment(lang_def::Range {
            start: "//".into(),
            end: "\n".into(),
            exceptions: Vec::new(),
            eof_allowed: true,
        }));
    lang.range_based
        .push(lang_def::RangeBased::String(lang_def::Range {
            start: "\"".into(),
            end: "\"".into(),
            exceptions: Vec::new(),
            eof_allowed: false,
        }));

    lang.special_sym.append(
        &mut ["::=", "|", "<", ">", "(", ")", "*", "?", "+", ";"]
            .iter()
            .map(|s| s.to_string())
            .collect(),
    );
    let (tokens, errors) = lexer::tokenize(&lang, grammar);
    if errors.len() > 0 {
        let err_msg = errors
            .iter()
            .map(|e| e.msg.clone())
            .collect::<Vec<String>>()
            .join("\n");
        return Err(format!("BNF parsing error(s): {}", err_msg));
    }
    // this is going to be quick & dirty parsing attempt since
    // a correct input is expected - it's just an internal stuff
    let mut bnf = BNF { rules: Vec::new() };
    // let mut iter = tokens.iter();
    let mut iter = ParseIter::new(&tokens);
    let mut line = 1;
    iter.next();
    while let Some(t) = iter.peek() {
        if t.kind == lexer::TokenKind::Comment {
            line += 1;
            iter.next();
            continue;
        }
        // we're expecting non-terminal here
        let non_terminal = get_non_terminal(&mut iter, line)?;
        trace!("parse: got lhs symbol: {}", non_terminal);
        // we're expecting '::=' symbol now
        if let Some(t) = iter.peek() {
            if t.kind != lexer::TokenKind::Symbol || t.text != "::=" {
                return Err(err_exp_sym("::=", &t.text, line));
            }
            iter.next();
        } else {
            return Err(err_eof());
        }
        // now get symbols
        let symbols = get_symbols(&mut iter, line)?;

        bnf.rules.push(Rule {
            non_terminal,
            symbols,
        });
        line += 1;
    }

    Ok(bnf)
}

fn err_exp_sym(sym: &str, got: &str, line: usize) -> String {
    format!(
        "BNF syntax error at {} line/rule#: expected symbol '{}', got '{}'",
        line, sym, got
    )
}

fn err_eof() -> String {
    "BNF syntax error: unexpected end of program".into()
}

fn parse_term(iter: &mut Iter, line: usize) -> Result<Symbol, String> {
    if let Some(t) = iter.peek() {
        trace!("parse_term starts at {}", t);
        if t.kind == lexer::TokenKind::String {
            iter.next();
            Ok(get_symbols_count(
                Symbol::Terminal(t.text.clone()),
                iter,
                line,
            ))
        } else {
            Err(format!(
                "BNF syntax error at {} line/rule#: expected terminal, got {}",
                line, t.text
            ))
        }
    } else {
        Err(err_eof())
    }
}

fn parse_nterm(iter: &mut Iter, line: usize) -> Result<Symbol, String> {
    trace!("parse_nterm starts");
    let nonterm = get_non_terminal(iter, line)?;
    Ok(get_symbols_count(nonterm, iter, line))
}

fn parse_group(iter: &mut Iter, line: usize) -> Result<Symbol, String> {
    if let Some(t) = iter.peek() {
        trace!("parse_group starts at {}", t);
        if t.kind == lexer::TokenKind::Symbol && t.text == "(" {
            iter.next();
            let symbols = parse_rhs(iter, line)?;
            if let Some(tt) = iter.peek() {
                if tt.kind == lexer::TokenKind::Symbol && tt.text == ")" {
                    iter.next();
                    return Ok(get_symbols_count(Symbol::Group(symbols), iter, line));
                }
                return Err(format!("BNF syntax error at {} line/rule#: parsing group failed - expected closing ')', got: '{}:{}'", line, tt.kind, tt.text));
            } else {
                return Err(err_eof());
            }
        }
        Err(format!(
            "BNF syntax error at {} line/rule#: expected '(', got '{}:{}",
            line, t.kind, t.text
        ))
    } else {
        Err(err_eof())
    }
}

fn get_symbols_count(current_sym: Symbol, iter: &mut Iter, _line: usize) -> Symbol {
    if let Some(next) = iter.peek() {
        if next.kind == lexer::TokenKind::Symbol {
            match next.text.as_str() {
                "*" => {
                    iter.next();
                    return Symbol::ZeroOrMore(Box::new(current_sym));
                }
                "?" => {
                    iter.next();
                    return Symbol::ZeroOrOne(Box::new(current_sym));
                }
                "+" => {
                    iter.next();
                    return Symbol::AtLeastOnce(Box::new(current_sym));
                }
                _ => {}
            }
        }
    }
    current_sym
}

fn parse_symbol(iter: &mut Iter, line: usize) -> Result<Symbol, String> {
    if let Some(t) = iter.peek() {
        trace!("parse_symbol starts at {}", t);
        match t.kind {
            lexer::TokenKind::String => {
                // string so this is a terminal
                return Ok(parse_term(iter, line)?);
            }
            lexer::TokenKind::Symbol => match t.text.as_str() {
                "(" => {
                    return Ok(parse_group(iter, line)?);
                }
                "<" => {
                    // this must be a non-terminal
                    return Ok(parse_nterm(iter, line)?);
                }
                _ => {
                    return Err(format!(
                        "BNF syntax error at {} line/rule#: expected BNF symbol (terminal, nonterminal or group) but got unexpected token '{}:{}'",
                        line, t.kind, t.text
                    ));
                }
            },
            _ => {
                return Err(format!(
                    "BNF syntax error at {} line/rule#: expected BNF symbol but got unexpected token '{}:{}'",
                    line, t.kind, t.text
                ));
            }
        }
    }
    Err(err_eof())
}

fn parse_rhs(iter: &mut Iter, line: usize) -> Result<Vec<Symbol>, String> {
    if let Some(t) = iter.peek() {
        trace!("parse_rhs starting at: {}", t);
        if t.kind == lexer::TokenKind::Symbol {
            if t.text == ";" {
                return Ok(vec![]);
            } else if t.text == ")" {
                return Ok(vec![]);
            }
        }
    } else {
        return Err(err_eof());
    }
    let mut symbols = Vec::new();
    trace!("parse_rhs: looking for first symbol...");
    let mut prev_sym = parse_symbol(iter, line)?;
    trace!("parse_rhs: got first symbol: {}", prev_sym);
    while let Some(t) = iter.peek() {
        trace!("parse_rhs: current symbol: {}", t);
        if t.kind == lexer::TokenKind::Symbol {
            match t.text.as_str() {
                ";" => {
                    break;
                }
                "|" => {
                    iter.next();
                    prev_sym = Symbol::Or(vec![prev_sym, parse_symbol(iter, line)?]);
                    continue;
                }
                ")" => {
                    break;
                }
                _ => {}
            }
        }
        symbols.push(prev_sym);
        trace!("parse_rhs: looking for next symbol...");
        prev_sym = parse_symbol(iter, line)?;
        trace!("parse_rhs: got next symbol: {}", prev_sym);
    }
    symbols.push(prev_sym);
    trace!("parse_rhs: parsing last symbols...");
    symbols.append(&mut parse_rhs(iter, line)?);
    trace!("parse_rhs: parsing last symbols done");
    Ok(symbols)
}

fn get_symbols(iter: &mut Iter, line: usize) -> Result<Vec<Symbol>, String> {
    let syms = parse_rhs(iter, line)?;
    // expecting ';'
    if let Some(t) = iter.peek() {
        if t.text == ";" {
            iter.next();
        }
    }
    Ok(syms)
}

fn get_non_terminal(iter: &mut Iter, line: usize) -> Result<Symbol, String> {
    if let Some(current) = iter.peek() {
        trace!("get_non_terminal: started at {}", current);
        let name;
        if current.text != "<" {
            return Err(err_exp_sym("<", &current.text, line));
        }
        if let Some(t) = iter.next() {
            if t.kind != lexer::TokenKind::Literal {
                return Err(format!(
                "BNF syntax error at {} line/rule#: expected non-terminal name (literal), got {} ({})",
                line, t.text, t.kind
            ));
            } else {
                name = t.text.clone();
            }
        } else {
            return Err(err_eof());
        }
        if let Some(t) = iter.next() {
            if t.text != ">" {
                return Err(err_exp_sym(">", &t.text, line));
            }
            iter.next();
        } else {
            return Err(err_eof());
        }
        Ok(Symbol::NonTerminal(name))
    } else {
        Err(err_eof())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_grammar() {
        let grammar = "";
        let r = parse(grammar);
        assert!(r.is_ok());
        assert_eq!(r.unwrap().rules.len(), 0);
    }

    #[test]
    fn test_simple_grammar() {
        let grammar = r#"
// this is a simple grammar
<PROGRAM> ::= "let" <VAR_NAME> "=" <EXPR>;
<VARNAME> ::= "STR_LITERAL";
<EXPR> ::= "STR_LITERAL";
        "#;

        let r = parse(grammar);
        if let Err(e) = &r {
            println!("Error while parsing BNF: {}", e);
        }
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.rules.len(), 3);
    }
    #[test]
    fn test_groupings() {
        let grammar = r#"
<program> ::= "1" <2>? "3" (<4>* <5>) (<6> <7>)+;
        "#;

        let r = parse(grammar);
        if let Err(e) = &r {
            println!("Error while parsing BNF: {}", e);
        }
        assert!(r.is_ok());
        let r = r.unwrap();
        assert_eq!(r.rules.len(), 1);
    }

    #[test]
    fn test_different_cases_one_rule() {
        let tcs = [
            (
                r#"<prog> ::= "if"  <STATEMENT>;"#,
                Symbol::NonTerminal("prog".into()),
                vec![
                    Symbol::Terminal("if".into()),
                    Symbol::NonTerminal("STATEMENT".into()),
                ],
            ),
            (
                r#"<p> ::= <1> | (<2> <3>);"#,
                Symbol::NonTerminal("p".into()),
                vec![Symbol::Or(vec![
                    Symbol::NonTerminal("1".into()),
                    Symbol::Group(vec![
                        Symbol::NonTerminal("2".into()),
                        Symbol::NonTerminal("3".into()),
                    ]),
                ])],
            ),
            (
                r#"<p> ::= <1> | (<2> | <3>)+ | <4>;"#,
                Symbol::NonTerminal("p".into()),
                vec![Symbol::Or(vec![
                    Symbol::Or(vec![
                        Symbol::NonTerminal("1".into()),
                        Symbol::AtLeastOnce(Box::new(Symbol::Group(vec![Symbol::Or(vec![
                            Symbol::NonTerminal("2".into()),
                            Symbol::NonTerminal("3".into()),
                        ])]))),
                    ]),
                    Symbol::NonTerminal("4".into()),
                ])],
            ),
        ];

        for t in tcs {
            println!("Testing {}...", t.0);
            let r = parse(t.0);
            if let Err(e) = &r {
                println!("Error: {}", e);
            }
            assert!(r.is_ok());
            let r = r.unwrap();
            assert_eq!(r.rules.len(), 1);

            for s in &r.rules[0].symbols {
                println!("Symbols: {}", s);
            }

            assert_eq!(r.rules[0].non_terminal, t.1);
            assert_eq!(r.rules[0].symbols.len(), t.2.len());
            for (s, exp) in r.rules[0].symbols.iter().zip(t.2) {
                assert_eq!(*s, exp);
            }
        }
    }
}
