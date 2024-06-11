// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 11.06.2024
// ---------------------------------------------------

// Backus–Naur form grammar parser
use crate::lang_def;
use crate::lexer;
use std::boxed::Box;

/*
* Rules:
* - non-terminals must be enclosed in brackets <nonterminal>
* - Terminals must be surrounded in "quotes"
* - Epsilon (which matches nothing) can be sybolized by a capital E. For example, <very> ::= E | "very" <very> matches zero or more "very"s in a row, such as veryveryvery (though 'zero or more' can be written more easily using the EBNF '*').
* - Comments: a valid comment start with '//' and ends with a new line

Simplified BNF grammar for BNF parser
<symbols> ::= <term> <symbols> | <nterm> <symbols> | <group> <symbols> | <or_case> | E
<term> ::= <term_s> <cnt>
<term_s> ::= "\"" [a-z] "\""
<nterm> ::= <nterm_s> <cnt>
<nterm_s> ::= "<" [a-z] ">"
<group> ::= <group_s> <cnt>
<group_s> ::= "(" <symbols> ")"

<or_case> ::= <term> "|" <symbols> | <nterm> "|" <symbols> | <group> "|" <symbols>

<cnt> ::= "?" | "*" | "+" | E
*/

pub enum Symbol {
    Terminal(String),
    NonTerminal(String),
    Group(Vec<Symbol>),
    OrGroup(Vec<Symbol>),
    ZeroOrMore(Box<Symbol>),
    AtLeastOnce(Box<Symbol>),
    ZeroOrOne(Box<Symbol>),
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
    let mut iter = tokens.iter();
    let mut line = 1;
    while let Some(t) = iter.next() {
        if t.kind == lexer::TokenKind::Comment {
            line += 1;
            continue;
        }
        // we're expecting non-terminal here
        let non_terminal = get_non_terminal(&t, &mut iter, line)?;
        // we're expecting '::=' symbol now
        if let Some(t) = iter.next() {
            if t.kind != lexer::TokenKind::Symbol || t.text != "::=" {
                return Err(err_exp_sym("::=", &t.text, line));
            }
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

fn get_symbols_term(
    t: &lexer::Token,
    iter: &mut std::slice::Iter<lexer::Token>,
    line: usize,
) -> Result<Symbol, String> {
    if t.kind == lexer::TokenKind::String {
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
}

fn get_symbols_nonterm(
    t: &lexer::Token,
    iter: &mut std::slice::Iter<lexer::Token>,
    line: usize,
) -> Result<Symbol, String> {
    let nonterm = get_non_terminal(t, iter, line)?;
    Ok(get_symbols_count(nonterm, iter, line))
}

fn get_symbols_group(
    t: &lexer::Token,
    iter: &mut std::slice::Iter<lexer::Token>,
    line: usize,
) -> Result<Symbol, String> {
    if t.kind == lexer::TokenKind::Symbol && t.text == "(" {
        let symbols = get_symbols(iter, line)?;
        if let Some(tt) = iter.next() {
            if tt.kind == lexer::TokenKind::Symbol && tt.text == ")" {
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
}

fn get_symbols_or(
    t: &lexer::Token,
    iter: &mut std::slice::Iter<lexer::Token>,
    line: usize,
) -> Result<Symbol, String> {
    Err("".into())
}

fn get_symbols_count(
    current_sym: Symbol,
    iter: &mut std::slice::Iter<lexer::Token>,
    _line: usize,
) -> Symbol {
    let mut iter_cloned = iter.clone();
    if let Some(next) = iter_cloned.next() {
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

fn get_symbols(
    iter: &mut std::slice::Iter<lexer::Token>,
    line: usize,
) -> Result<Vec<Symbol>, String> {
    let mut symbols = Vec::new();
    if let Some(t) = iter.clone().next() {
        match t.kind {
            lexer::TokenKind::String => {
                // string so this is a terminal
                iter.next();
                symbols.push(get_symbols_term(&t, iter, line)?);
                for s in get_symbols(iter, line)? {
                    symbols.push(s);
                }
            }
            lexer::TokenKind::Symbol => match t.text.as_str() {
                "(" => {
                    iter.next();
                    // group begins
                    let nt = get_symbols_group(&t, iter, line)?;
                    symbols.push(nt);
                    for s in get_symbols(iter, line)? {
                        symbols.push(s);
                    }
                }
                ";" => {
                    iter.next();
                }
                ")" => {}
                "<" => {
                    // this must be a non-terminal
                    iter.next();
                    let nt = get_symbols_nonterm(&t, iter, line)?;
                    symbols.push(nt);
                    for s in get_symbols(iter, line)? {
                        symbols.push(s);
                    }
                }
                _ => {
                    return Err(format!(
                        "BNF syntax error at {} line/rule#: unknown symbol '{}'",
                        line, t.text
                    ));
                }
            },
            _ => {
                return Err(format!(
                    "BNF syntax error at {} line/rule#: unexpected symbol '{}:{}'",
                    line, t.kind, t.text
                ));
            }
        }
    }
    Ok(symbols)
}

fn get_non_terminal(
    current: &lexer::Token,
    iter: &mut std::slice::Iter<lexer::Token>,
    line: usize,
) -> Result<Symbol, String> {
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
    } else {
        return Err(err_eof());
    }
    Ok(Symbol::NonTerminal(name))
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
}
