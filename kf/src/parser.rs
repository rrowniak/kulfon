// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------
use crate::ast;
use crate::lang_def::{KfTokKind, KfToken, ParsingError};
use crate::lexer;
use crate::parse_iter::ParseIter;

type ParsingErrs = Vec<ParsingError>;
type ParsingResult<T> = Result<T, ParsingErrs>;

fn convert_tok_to_kftok(tokens: &Vec<lexer::Token>) -> (Vec<KfToken>, ParsingErrs) {
    let mut kftokens = Vec::new();
    let errors = Vec::new();

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
            // lexer::TokenKind::Comment => KfTokKind::Comment,
            lexer::TokenKind::Comment =>continue,
        };
        kftokens.push(KfToken {
            kind: new_kind,
            text: t.text.clone(),
            at: t.start,
        });
    }

    (kftokens, errors)
}

pub fn parse(tokens: &Vec<lexer::Token>) -> ParsingResult<ast::Node> {
    let (tokens, errors) = convert_tok_to_kftok(tokens);
    if errors.len() > 0 {
        return Err(errors);
    }
    let mut kfparser = KfParser::new(&tokens);
    kfparser.parse_prog()
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

    fn parse_prog(&mut self) -> ParsingResult<ast::Node> {
        self.iter.next();
        let mut global_scope = Vec::new();
        while self.iter.peek().is_some() {
            global_scope.push(self.parse_prog_statement()?);
        }
        Ok(ast::Node {
            val: ast::Ntype::Scope(global_scope),
        })
    }

    fn parse_prog_statement(&mut self) -> ParsingResult<ast::Node> {
        match &self.get_curr()?.kind {
            KfTokKind::KwFn => {
                // function declaration
                self.parse_fun()
            }
            KfTokKind::KwLet => {
                if self.check_tok_sequence(&[KfTokKind::KwLet, KfTokKind::KwMut]) {
                    self.parse_mut_var()
                } else {
                    self.parse_var()
                }
            }
            t => {
                // error: unexpected symbol
                panic!("Unexpected symbol: {:?}", t);
            }
        }
    }

    fn parse_fun(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwFn)?;
        let name = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::SymParenthOpen)?;
        let args = self.parse_arg_list()?;
        self.consume_tok(KfTokKind::SymParenthClose)?;
        // return value specified?
        let ret = if self.check_current_tok(KfTokKind::SymArrow) {
            self.consume_tok(KfTokKind::SymArrow)?;
            self.parse_type()?
        } else {
            ast::TypeDecl {
                typename: String::new(),
            }
        };
        let body = self.parse_scope()?;
        Ok(ast::Node {
            val: ast::Ntype::FnDef(ast::Fun {
                name,
                args,
                ret,
                body: Box::new(body),
            }),
        })
    }

    fn parse_arg_list(&mut self) -> ParsingResult<Vec<ast::VarDecl>> {
        let mut ret = Vec::new();
        if !self.check_current_tok(KfTokKind::SymParenthClose) {
            let name = self.consume_name_literal()?;
            self.consume_tok(KfTokKind::SymColon)?;
            let type_dec = self.parse_type()?;
            ret.push(ast::VarDecl { name, type_dec });
        }
        while !self.check_current_tok(KfTokKind::SymParenthClose) {
            if self.match_current_tok(KfTokKind::SymComma) {
                let name = self.consume_name_literal()?;
                self.consume_tok(KfTokKind::SymColon)?;
                let type_dec = self.parse_type()?;
                ret.push(ast::VarDecl { name, type_dec });
            } else {
                return Err(error_from_token(
                    self.get_curr()?,
                    "Unexpected token while parsing function argument list",
                ));
            }
        }
        Ok(ret)
    }

    fn parse_type(&mut self) -> ParsingResult<ast::TypeDecl> {
        let typename = self.consume_name_literal()?;
        Ok(ast::TypeDecl { typename })
    }

    fn parse_scope(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::SymCurlyOpen)?;
        let mut scope = Vec::new();
        while !self.check_current_tok(KfTokKind::SymCurlyClose) {
            match self.get_curr()?.kind {
                KfTokKind::KwIf | KfTokKind::KwFor | KfTokKind::KwWhile | KfTokKind::KwLoop => {
                    scope.push(self.parse_ctrl_flow()?)
                }
                KfTokKind::KwLet => scope.push(self.parse_var()?),
                _ => {
                    scope.push(self.parse_expression()?);
                    self.consume_tok(KfTokKind::SymSemi)?;
                }
            }
        }
        self.consume_tok(KfTokKind::SymCurlyClose)?;
        Ok(ast::Node {
            val: ast::Ntype::Scope(scope),
        })
    }

    fn parse_expression(&mut self) -> ParsingResult<ast::Node> {
        Ok(self.parse_equality()?)
    }

    fn parse_equality(&mut self) -> ParsingResult<ast::Node> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_current_tok(KfTokKind::OpEq) {
                let right = self.parse_comparison()?;
                expr = ast::Node {
                    val: ast::Ntype::Eq(Box::new(expr), Box::new(right)),
                };
                continue;
            } else if self.match_current_tok(KfTokKind::OpNe) {
                let right = self.parse_comparison()?;
                expr = ast::Node {
                    val: ast::Ntype::Neq(Box::new(expr), Box::new(right)),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParsingResult<ast::Node> {
        let mut expr = self.parse_term()?;
        loop {
            if self.match_current_tok(KfTokKind::OpGt) {
                let right = self.parse_term()?;
                expr = ast::Node {
                    val: ast::Ntype::Gt(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpGe) {
                let right = self.parse_term()?;
                expr = ast::Node {
                    val: ast::Ntype::Ge(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpLt) {
                let right = self.parse_term()?;
                expr = ast::Node {
                    val: ast::Ntype::Lt(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpLe) {
                let right = self.parse_term()?;
                expr = ast::Node {
                    val: ast::Ntype::Le(Box::new(expr), Box::new(right)),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParsingResult<ast::Node> {
        let mut expr = self.parse_factor()?;
        loop {
            if self.match_current_tok(KfTokKind::OpPlus) {
                let right = self.parse_factor()?;
                expr = ast::Node {
                    val: ast::Ntype::Plus(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpMinus) {
                let right = self.parse_factor()?;
                expr = ast::Node {
                    val: ast::Ntype::Minus(Box::new(expr), Box::new(right)),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParsingResult<ast::Node> {
        let mut expr = self.parse_unary()?;
        loop {
            if self.match_current_tok(KfTokKind::OpSlash) {
                let right = self.parse_unary()?;
                expr = ast::Node {
                    val: ast::Ntype::Slash(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpStar) {
                let right = self.parse_unary()?;
                expr = ast::Node {
                    val: ast::Ntype::Star(Box::new(expr), Box::new(right)),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParsingResult<ast::Node> {
        if self.match_current_tok(KfTokKind::OpBang) {
            let expr = self.parse_unary()?;
            return Ok(ast::Node {
                val: ast::Ntype::Bang(Box::new(expr)),
            });
        } else if self.match_current_tok(KfTokKind::OpMinus) {
            let expr = self.parse_unary()?;
            return Ok(ast::Node {
                val: ast::Ntype::UMinus(Box::new(expr)),
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParsingResult<ast::Node> {
        let t = self.get_curr()?;
        if self.match_current_tok(KfTokKind::SlTrue) {
            return Ok(ast::Node {
                val: ast::Ntype::True,
            });
        } else if self.match_current_tok(KfTokKind::SlFalse) {
            return Ok(ast::Node {
                val: ast::Ntype::False,
            });
        } else if self.check_tok_sequence(&[KfTokKind::Literal, KfTokKind::SymParenthOpen]) {
            // this is a function call
            return self.parse_fn_call();
        } else if self.match_current_tok(KfTokKind::LitChar) {
            return Ok(ast::Node {
                val: ast::Ntype::Char(t.text.clone()),
            });
        } else if self.match_current_tok(KfTokKind::LitString) {
            return Ok(ast::Node {
                val: ast::Ntype::String(t.text.clone()),
            });
        } else if self.match_current_tok(KfTokKind::Literal) {
            return Ok(ast::Node {
                val: ast::Ntype::Literal(t.text.clone()),
            });
        } else if self.match_current_tok(KfTokKind::SymParenthOpen) {
            let expr = self.parse_expression()?;
            self.consume_tok(KfTokKind::SymParenthClose)?;
            return Ok(expr);
        }

        Err(error_from_token(
            t,
            "Unexpected token while parsing expression",
        ))
    }

    fn parse_fn_call(&mut self) -> ParsingResult<ast::Node> {
        let fn_name = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::SymParenthOpen)?;
        let args = self.parse_expr_list()?;
        self.consume_tok(KfTokKind::SymParenthClose)?;
        return Ok(ast::Node {
            val: ast::Ntype::FnCall(fn_name, args),
        });
    }

    fn parse_expr_list(&mut self) -> ParsingResult<Vec<ast::Node>> {
        let mut ret = Vec::new();
        // expression list can be empty
        if !self.check_current_tok(KfTokKind::SymParenthClose) {
            ret.push(self.parse_expression()?);
        }
        while !self.check_current_tok(KfTokKind::SymParenthClose) {
            if self.match_current_tok(KfTokKind::SymComma) {
                ret.push(self.parse_expression()?);
            } else {
                return Err(error_from_token(
                    self.get_curr()?,
                    "Unexpected token while parsing expression list",
                ));
            }
        }
        Ok(ret)
    }

    fn parse_ctrl_flow(&mut self) -> ParsingResult<ast::Node> {
        match self.get_curr()?.kind {
            KfTokKind::KwIf => self.parse_if(),
            KfTokKind::KwFor => self.parse_for(),
            KfTokKind::KwWhile => self.parse_while(),
            KfTokKind::KwLoop => self.parse_loop(),
            _ => Err(error_from_token(
                self.get_curr()?,
                "Unexpected token while parsing control flow",
            )),
        }
    }

    fn parse_if(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwIf)?;
        let cond = Box::new(self.parse_expression()?);
        let body = Box::new(self.parse_scope()?);
        let mut elif = Vec::new();
        while self.check_tok_sequence(&[KfTokKind::KwElse, KfTokKind::KwIf]) {
            self.consume_tok(KfTokKind::KwElse)?;
            self.consume_tok(KfTokKind::KwIf)?;
            let cond = Box::new(self.parse_expression()?);
            let body = Box::new(self.parse_scope()?);
            elif.push(ast::Elif { cond, body });
        }
        let else_body = if self.match_current_tok(KfTokKind::KwElse) {
            Some(Box::new(self.parse_scope()?))
        } else {
            None
        };
        Ok(ast::Node {
            val: ast::Ntype::If(ast::If {
                cond,
                body,
                elif,
                else_body,
            }),
        })
    }

    fn parse_for(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwFor)?;
        let var_pattern = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::KwIn)?;
        let in_expr = Box::new(self.parse_expression()?);
        let body = Box::new(self.parse_scope()?);
        Ok(ast::Node {
            val: ast::Ntype::For(ast::For {
                var_pattern,
                in_expr,
                body,
            }),
        })
    }

    fn parse_while(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwWhile)?;
        let cond = Box::new(self.parse_expression()?);
        let body = Box::new(self.parse_scope()?);
        Ok(ast::Node {
            val: ast::Ntype::While(ast::While { cond, body }),
        })
    }

    fn parse_loop(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwLoop)?;
        let body = Box::new(self.parse_scope()?);
        Ok(ast::Node {
            val: ast::Ntype::Loop(ast::Loop { body }),
        })
    }

    fn parse_var(&mut self) -> ParsingResult<ast::Node> {
        if self.check_tok_sequence(&[KfTokKind::KwLet, KfTokKind::KwMut]) {
            self.parse_mut_var()
        } else {
            self.parse_const_var()
        }
    }

    fn parse_const_var(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwLet)?;
        let name = self.consume_name_literal()?;
        let vartype = if self.match_current_tok(KfTokKind::SymColon) {
            Some(ast::TypeDecl {
                typename: self.consume_name_literal()?,
            })
        } else {
            None
        };
        let expr = if self.match_current_tok(KfTokKind::OpAssign) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        self.consume_tok(KfTokKind::SymSemi)?;
        Ok(ast::Node {
            val: ast::Ntype::VarDef(ast::VarDef {
                mutable: false,
                name,
                vartype,
                expr,
            }),
        })
    }

    fn parse_mut_var(&mut self) -> ParsingResult<ast::Node> {
        self.consume_tok(KfTokKind::KwLet)?;
        self.consume_tok(KfTokKind::KwMut)?;
        let name = self.consume_name_literal()?;
        let vartype = if self.match_current_tok(KfTokKind::SymColon) {
            Some(ast::TypeDecl {
                typename: self.consume_name_literal()?,
            })
        } else {
            None
        };
        let expr = if self.match_current_tok(KfTokKind::OpAssign) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };
        self.consume_tok(KfTokKind::SymSemi)?;
        Ok(ast::Node {
            val: ast::Ntype::VarDef(ast::VarDef {
                mutable: true,
                name,
                vartype,
                expr,
            }),
        })
    }

    /// Returns current token or emits eof error
    fn get_curr(&self) -> ParsingResult<&'a KfToken> {
        match self.iter.peek() {
            Some(t) => Ok(t),
            None => Err(error_eof()),
        }
    }

    /// Moves current token to the next position only if
    /// current token matches provided `kind`. Otherwise
    /// an error is reported.
    fn consume_tok(&mut self, kind: KfTokKind) -> ParsingResult<()> {
        let t = self.iter.peek();
        match t {
            Some(t) => {
                if t.kind == kind {
                    self.iter.next();
                    return Ok(());
                } else {
                    let msg = format!("Expected {:?}, got {:?}", kind, t);
                    return Err(error_from_token(t, &msg));
                }
            }
            None => return Err(error_eof()),
        }
    }
    /// Check if the current token is equal to `kind`
    /// If so, consume it and return true.
    /// Otherwise return false.
    fn match_current_tok(&mut self, kind: KfTokKind) -> bool {
        if self.check_current_tok(kind) {
            self.iter.next();
            return true;
        }
        return false;
    }

    /// Check if the current token is equal to `kind`
    fn check_current_tok(&self, kind: KfTokKind) -> bool {
        let t = self.iter.peek();
        if t.is_some() && t.unwrap().kind == kind {
            return true;
        }
        return false;
    }

    fn check_tok_sequence(&self, kinds: &[KfTokKind]) -> bool {
        for (i, kind) in kinds.iter().enumerate() {
            let t = self.iter.lookahead(i);
            if !t.is_some() {
                return false;
            }
            let t = t.unwrap();
            if kind != &t.kind {
                return false;
            }
        }
        true
    }

    /// Returns `text` of current token and moves
    /// iterator to the next position only if
    /// current token is literal.
    /// Otherwise an error is reported.
    fn consume_name_literal(&mut self) -> ParsingResult<String> {
        let t = self.iter.peek();
        match t {
            Some(t) => {
                if t.kind == KfTokKind::Literal {
                    self.iter.next();
                    return Ok(t.text.clone());
                } else {
                    let msg = format!("Expected literal, got {:?}", t.kind);
                    return Err(error_from_token(t, &msg));
                }
            }
            None => return Err(error_eof()),
        }
    }
}

fn error_from_token(t: &KfToken, msg: &str) -> ParsingErrs {
    vec![ParsingError {
        msg: format!("{}", msg),
        details: format!("Token {:?}", t),
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
    use crate::lang_def::Lang;

    fn throw_errors(errs: ParsingErrs) -> Result<(), String> {
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

    fn parse_code(code: &str) -> Result<(), String> {
        // let bnf = bnf_parser::parse(BNF_GRAMMAR)?;
        // tokenize code
        let (tokens, errors) = lexer::tokenize(&Lang::new(), code);
        throw_errors(errors)?;
        let (tokens, errors) = convert_tok_to_kftok(&tokens);
        throw_errors(errors)?;
        let mut kfparser = KfParser::new(&tokens);
        let _ast = match kfparser.parse_prog() {
            Ok(ast) => ast,
            Err(errs) => {
                let mut err_msg = String::new();
                for e in errs {
                    err_msg += code;
                    err_msg += &format!("\n{}^\n", " ".repeat(e.at.col - 1));
                    err_msg += &format!("Syntax error: {}", e.msg)
                }
                return Err(err_msg);
            }
        };
        Ok(())
    }

    #[test]
    fn test_hello_word() {
        let kulfon_code = r#"
        fn main() {
            println("hello word");
        }
        "#;
        let result = parse_code(kulfon_code);
        match result {
            Ok(_) => {}
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }

    #[test]
    fn parse_correct_code() {
        let tcs = [
            "fn main(){}",
            "fn _fn() {}",
            "let variable = 10;",
            "let mut _xyz: u32 = 1 + 2;",
            "let mut a;",
            "fn abc(a: usize, b: i32, c: bool) { println(a, b, c, 3 / 8); }",
            "fn abc() { if x >= 9 { println(x);} else {println(y);}}",
            "fn abc() { if x == y { println(x); if x == true {}} else if cc==8 {println(y);}}",
            "fn a() { while x < 10 {println(x);}}",
        ];
        for c in tcs {
            match parse_code(c) {
                Ok(_) => {}
                Err(e) => {
                    println!("Parsing: {}", c);
                    println!("{:}", e);
                    assert!(false);
                }
            }
        }
    }
}
