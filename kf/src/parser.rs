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

type ParsingErrs = Vec<ParsingError>;
type ParsingResult<T> = Result<T, ParsingErrs>;

fn convert_tok_to_kftok(tokens: Vec<lexer::Token>) -> (Vec<KfToken>, ParsingErrs) {
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
            let msg = format!("Syntax error at {}, {}: {}", e.at.line, e.at.col, e.msg);
            error.push_str(&msg);
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
        Ok(ast::Fun {
            name,
            args,
            ret,
            body,
        })
    }

    fn parse_arg_list(&mut self) -> ParsingResult<Vec<ast::VarDecl>> {
        Ok(Vec::new())
    }

    fn parse_type(&mut self) -> ParsingResult<ast::TypeDecl> {
        let typename = self.consume_name_literal()?;
        Ok(ast::TypeDecl { typename })
    }

    fn parse_scope(&mut self) -> ParsingResult<ast::Scope> {
        self.consume_tok(KfTokKind::SymCurlyOpen)?;
        let mut scope = ast::Scope { exprs: Vec::new() };
        while !self.check_current_tok(KfTokKind::SymCurlyClose) {
            scope.exprs.push(self.parse_expression()?);
            self.consume_tok(KfTokKind::SymSemi)?;
        }
        self.consume_tok(KfTokKind::SymCurlyClose)?;
        Ok(scope)
    }

    fn parse_expression(&mut self) -> ParsingResult<ast::Expression> {
        Ok(self.parse_equality()?)
    }

    fn parse_equality(&mut self) -> ParsingResult<ast::Expression> {
        let mut expr = self.parse_comparison()?;
        loop {
            if self.match_current_tok(KfTokKind::OpEq) {
                let right = self.parse_comparison()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Eq(Box::new(expr), Box::new(right)),
                };
                continue;
            } else if self.match_current_tok(KfTokKind::OpNe) {
                let right = self.parse_comparison()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Neq(Box::new(expr), Box::new(right)),
                };
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self) -> ParsingResult<ast::Expression> {
        let mut expr = self.parse_term()?;
        loop {
            if self.match_current_tok(KfTokKind::OpGt) {
                let right = self.parse_term()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Gt(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpGe) {
                let right = self.parse_term()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Ge(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpLt) {
                let right = self.parse_term()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Lt(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpLe) {
                let right = self.parse_term()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Le(Box::new(expr), Box::new(right)),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self) -> ParsingResult<ast::Expression> {
        let mut expr = self.parse_factor()?;
        loop {
            if self.match_current_tok(KfTokKind::OpPlus) {
                let right = self.parse_factor()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Plus(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpMinus) {
                let right = self.parse_factor()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Minus(Box::new(expr), Box::new(right)),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParsingResult<ast::Expression> {
        let mut expr = self.parse_unary()?;
        loop {
            if self.match_current_tok(KfTokKind::OpSlash) {
                let right = self.parse_unary()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Slash(Box::new(expr), Box::new(right)),
                };
            } else if self.match_current_tok(KfTokKind::OpStar) {
                let right = self.parse_unary()?;
                expr = ast::Expression {
                    expr: ast::ExprNode::Star(Box::new(expr), Box::new(right)),
                };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParsingResult<ast::Expression> {
        if self.match_current_tok(KfTokKind::OpBang) {
            let expr = self.parse_unary()?;
            return Ok(ast::Expression {
                expr: ast::ExprNode::Bang(Box::new(expr)),
            });
        } else if self.match_current_tok(KfTokKind::OpMinus) {
            let expr = self.parse_unary()?;
            return Ok(ast::Expression {
                expr: ast::ExprNode::UMinus(Box::new(expr)),
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> ParsingResult<ast::Expression> {
        let t = match self.iter.peek() {
            Some(t) => t,
            None => return Err(error_eof()),
        };

        if self.match_current_tok(KfTokKind::SlTrue) {
            return Ok(ast::Expression {
                expr: ast::ExprNode::True,
            });
        } else if self.match_current_tok(KfTokKind::SlFalse) {
            return Ok(ast::Expression {
                expr: ast::ExprNode::False,
            });
        } else if self.check_tok_sequence(&[KfTokKind::Literal, KfTokKind::SymParenthOpen]) {
            // this is a function call
            return self.parse_fn_call();
        } else if self.match_current_tok(KfTokKind::LitChar) {
            return Ok(ast::Expression {
                expr: ast::ExprNode::Char(t.text.clone()),
            });
        } else if self.match_current_tok(KfTokKind::LitString) {
            return Ok(ast::Expression {
                expr: ast::ExprNode::String(t.text.clone()),
            });
        } else if self.match_current_tok(KfTokKind::Literal) {
            return Ok(ast::Expression {
                expr: ast::ExprNode::Literal(t.text.clone()),
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

    fn parse_fn_call(&mut self) -> ParsingResult<ast::Expression> {
        let fn_name = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::SymParenthOpen)?;
        let args = self.parse_expr_list()?;
        self.consume_tok(KfTokKind::SymParenthClose)?;
        return Ok(ast::Expression {
            expr: ast::ExprNode::FnCall(fn_name, args),
        });
    }

    fn parse_expr_list(&mut self) -> ParsingResult<Vec<ast::Expression>> {
        let mut ret = Vec::new();
        // expression list can be empty
        while !self.check_current_tok(KfTokKind::SymParenthClose) {
            ret.push(self.parse_expression()?);
            if self.check_current_tok(KfTokKind::SymComma) {
                self.consume_tok(KfTokKind::SymComma)?;
            } else {
                // TODO: unexpected token
            }
        }
        Ok(ret)
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
                    let msg = format!("Expected literal, got {:?}", t);
                    return Err(error_from_token(t, &msg));
                }
            }
            None => return Err(error_eof()),
        }
    }
}

fn error_from_token(t: &KfToken, msg: &str) -> ParsingErrs {
    vec![ParsingError {
        msg: format!("{} ({:?})", msg, t),
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
        match result {
            Ok(r) => {
                println!("{:?}", r);
                // assert!(false);
            }
            Err(e) => {
                println!("{:?}", e);
                assert!(false);
            }
        }
    }
}
