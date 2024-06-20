// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------
// use crate::bnf_parser;
use crate::ast;
use crate::lang_def::{KfTokKind, KfToken, Lang, ParsingError};
use crate::lexer;
use crate::parse_iter::ParseIter;

const BNF_GRAMMAR: &str = r#"
<program> ::= <fun_def>;
<fun_def> ::= "fn" <FUN_IDENTIFIER> "(" <fn_args> ")" <fn_ret> "{" <fn_body> "}";

"#;

type ParsingErrs = Vec<ParsingError>;
type ParsingResult<T> = Result<T, ParsingErrs>;

fn convert_tok_to_kftok(tokens: Vec<lexer::Token>) -> (Vec<KfToken>, ParsingErrs) {
    let mut kftokens = Vec::new();
    let mut errors = Vec::new();

    for t in tokens {
        let new_kind = match t.kind {
            lexer::TokenKind::Keyword | lexer::TokenKind::Symbol => {
                let nk = KfTokKind::from(&t.text);
                let nk = match nk {
                    Some(nk) => nk,
                    None => {
                        let msg = format!("Lang definition error - couldn't recognise keyword or symbol for this token: {}", t);
                        panic!("{}", msg);
                    }
                };
                nk
            }
            lexer::TokenKind::String => KfTokKind::LitString,
            lexer::TokenKind::Literal => KfTokKind::Literal,
            lexer::TokenKind::Character => KfTokKind::LitChar,
            lexer::TokenKind::Comment => KfTokKind::Comment,
        };
        kftokens.push(KfToken {
            kind: new_kind,
            text: t.text,
            at: t.start,
        });
    }

    (kftokens, errors)
}

fn throw_errors(errs: ParsingErrs) -> Result<(), String> {
    if errs.len() == 0 {
        Ok(())
    } else {
        let mut error = String::new();
        for e in errs {
            error.push_str(&e.msg);
            error += "\n";
        }
        Err(error)
    }
}

pub fn parse(code: &str) -> Result<(), String> {
    // let bnf = bnf_parser::parse(BNF_GRAMMAR)?;
    // tokenize code
    let (tokens, errors) = lexer::tokenize(&Lang::new(), code);
    throw_errors(errors)?;
    let (tokens, errors) = convert_tok_to_kftok(tokens);
    throw_errors(errors)?;
    let mut kfparser = KfParser::new(&tokens);
    let ast = match kfparser.parse_prog() {
        Ok(ast) => ast,
        Err(errs) => {
            throw_errors(errs)?;
            panic!("That place should be unreachable.")
        }
    };
    println!("Ast: {:?}", ast);
    Ok(())
}

struct KfParser<'a> {
    iter: ParseIter<'a, KfToken>,
}

impl<'a> KfParser<'a> {
    fn new(tokens: &[KfToken]) -> KfParser {
        KfParser {
            iter: ParseIter::new(tokens),
        }
    }

    fn parse_prog(&mut self) -> ParsingResult<ast::GlobScope> {
        self.iter.next();
        let mut global_scope = ast::GlobScope {
            imports: Vec::new(),
            fns: Vec::new(),
        };
        loop {
            let t = self.iter.peek();
            if t.is_none() {
                break;
            }
            let t = t.unwrap();
            match t.kind {
                KfTokKind::KwFn => {
                    // function declaration
                    global_scope.fns.push(self.parse_fun()?);
                }
                _ => {
                    // error: unexpected symbol
                    panic!("Unexpected symbol: {:?}", t);
                }
            }
        }
        Ok(global_scope)
    }

    fn parse_fun(&mut self) -> ParsingResult<ast::Fun> {
        self.consume_tok(KfTokKind::KwFn)?;
        let fn_name = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::SymParenthOpen)?;
        let args = self.parse_arg_list()?;
        self.consume_tok(KfTokKind::SymParenthClose)?;
        // return value specified?
        let type = if self.check_this_tok(KfTokKind::SymArrow) {
            self.consume_tok(KfTokKind::SymArrow)?;
            self.parse_type()
        } else {
                None
            };
        self.parse_scope()?;
        Err(Vec::new())
    }

    fn consume_tok(&mut self, kind: KfTokKind) -> ParsingResult<()> {
        let t = self.iter.next();
        match t {
            Some(t) => {
                if t.kind == kind {
                    return Ok(());
                } else {
                    let msg = format!("Expected {:?}, got {:?}", kind, t);
                    return Err(error_from_token(t, &msg));
                }
            }
            None => return Err(error_eof()),
        }
    }

    fn check_this_tok(&self, kind: KfTokKind) -> bool {
        let t = self.iter.peek();
        if t.is_some() && t.unwrap().kind == kind {
            return true;
        }
        return false;
    }

    fn consume_name_literal(&mut self) -> ParsingResult<String> {
        let t = self.iter.next();
        match t {
            Some(t) => {
                if t.kind == KfTokKind::Literal {
                    return Ok(t.text.clone());
                } else {
                    let msg = format!("Expected function name, got {:?}", t);
                    return Err(error_from_token(t, &msg));
                }
            }
            None => return Err(error_eof()),
        }
    }
}

fn error_from_token(t: &KfToken, msg: &str) -> ParsingErrs {
    vec![ParsingError {
        msg: msg.into(),
        details: String::new(),
        at: t.at,
    }]
}

fn error_eof() -> ParsingErrs {
    use crate::lang_def::TextPoint;
    vec![ParsingError {
        msg: "Unexpected end of file".into(),
        details: String::new(),
        at: TextPoint { line: 0, col: 0 },
    }]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hello_word() {
        let kulfon_code = r#"
        fn main() {
            println("hello word");
        }
        "#;
        let result = parse(kulfon_code);
        assert!(result.is_ok());
    }
}
