// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------
use crate::ast;
use crate::comp_msg;
use crate::comp_msg::TextPoint;
use crate::lang_def::{KfTokKind, KfToken};
use crate::lexer;
use crate::parse_iter::ParseIter;

type ParsingErrs = comp_msg::CompileMsgCol;
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
            // TODO: Implement correct handling of comments, now they're just skipped
            // lexer::TokenKind::Comment => KfTokKind::Comment,
            lexer::TokenKind::Comment => continue,
        };
        kftokens.push(KfToken {
            kind: new_kind,
            text: t.text.into(),
            at: t.start,
        });
    }

    (kftokens, errors)
}

pub fn parse(tokens: &Vec<lexer::Token>) -> ParsingResult<ast::Tree> {
    let (tokens, errors) = convert_tok_to_kftok(tokens);
    if errors.len() > 0 {
        return Err(errors);
    }
    let mut kfparser = KfParser::new(&tokens);
    kfparser.parse_prog()
}

struct KfParser<'a> {
    iter: ParseIter<'a, KfToken>,
    // estimated AST tree size
    cap: usize,
}

impl<'a> KfParser<'a> {
    fn new(tokens: &[KfToken]) -> KfParser {
        KfParser {
            iter: ParseIter::new(tokens),
            cap: tokens.len()
        }
    }

    fn parse_prog(&mut self) -> ParsingResult<ast::Tree> {
        let mut tree = ast::Tree::new(self.cap);
        self.iter.next();
        let mut global_scope = ast::NodeRefs::new();
        while self.iter.peek().is_some() {
            global_scope.push(self.parse_prog_statement(&mut tree)?);
        }
        let root = ast::Node::new(
            ast::Ntype::Scope(global_scope),
            TextPoint { line: 0, col: 0 },
        );
        tree.root = tree.push(root);
            Ok(tree)
    }

    fn parse_prog_statement(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        match &self.get_curr()?.kind {
            KfTokKind::KwFn => self.parse_fun(tree),
            KfTokKind::KwLet => {
                if self.check_tok_sequence(&[KfTokKind::KwLet, KfTokKind::KwMut]) {
                    self.parse_mut_var(tree)
                } else {
                    self.parse_var(tree)
                }
            }
            KfTokKind::KwStruct => self.parse_struct(tree),
            KfTokKind::KwEnum => self.parse_enum(tree),
            KfTokKind::KwImpl => self.parse_impl(tree),
            _ => {
                // error: unexpected symbol
                Err(vec![comp_msg::error_inv_statement_glob()])
            }
        }
    }

    fn parse_fun(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwFn)?;
        let name = self.consume_name_literal()?;
        let mut variadic = false;
        self.consume_tok(KfTokKind::SymParenthOpen)?;
        let args = self.parse_arg_list(tree)?;
        if self.match_current_tok(KfTokKind::SymEllipsis) {
            variadic = true;
        }
        self.consume_tok(KfTokKind::SymParenthClose)?;
        // return value specified?
        let ret = if self.check_current_tok(KfTokKind::SymArrow) {
            self.consume_tok(KfTokKind::SymArrow)?;
            self.parse_type(tree)?
        } else {
            ast::TypeDecl::new(String::new())
        };
        let body = self.parse_scope(tree)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::FnDef(ast::Fun {
                name,
                args,
                variadic,
                ret,
                body,
            }),
            at,
        )))
    }

    fn parse_arg_list(&mut self, tree: &mut ast::Tree) -> ParsingResult<Vec<ast::VarDecl>> {
        let mut ret = Vec::new();
        if !self.check_current_tok(KfTokKind::SymParenthClose) {
            let name = self.consume_name_literal()?;
            self.consume_tok(KfTokKind::SymColon)?;
            let type_dec = self.parse_type(tree)?;
            ret.push(ast::VarDecl { name, type_dec });
        }
        while !self.check_current_tok(KfTokKind::SymParenthClose) {
            if self.check_tok_sequence(&[
                KfTokKind::SymComma,
                KfTokKind::SymEllipsis,
                KfTokKind::SymParenthClose,
            ]) {
                self.consume_tok(KfTokKind::SymComma)?;
                return Ok(ret);
            }
            if self.match_current_tok(KfTokKind::SymComma) {
                let name = self.consume_name_literal()?;
                self.consume_tok(KfTokKind::SymColon)?;
                let type_dec = self.parse_type(tree)?;
                ret.push(ast::VarDecl { name, type_dec });
            } else {
                return Err(vec![comp_msg::error_arg_list_expected_coma_or_parenth(
                    self.get_curr()?.at,
                )]);
            }
        }
        Ok(ret)
    }

    fn parse_type(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::TypeDecl> {
        // optimization - instead of calling recursively parse_type
        // for each '&', just loop over all '&' '& mut' and
        // get them out
        let mut scope_res_relative = true;
        let mut scope_resolution = Vec::new();
        let mut reference_stack = Vec::new();
        loop {
            if self.match_current_tok(KfTokKind::OpAnd) {
                // double ampersands are lexed as 'and' logic operator
                // && or && mut case
                reference_stack.push(ast::RefType::Borrow);
                let ref_type = if self.match_current_tok(KfTokKind::KwMut) {
                    ast::RefType::BorrowMut
                } else {
                    ast::RefType::Borrow
                };
                reference_stack.push(ref_type);
            } else if self.match_current_tok(KfTokKind::SymAmpersand) {
                let ref_type = if self.match_current_tok(KfTokKind::KwMut) {
                    ast::RefType::BorrowMut
                } else {
                    ast::RefType::Borrow
                };
                reference_stack.push(ref_type);
            } else {
                break;
            }
        }
        if self.match_current_tok(KfTokKind::SymBracketOpen) {
            // parse either array or slice
            // TODO: below call might fail, handle this error here and add
            // a context like: parsing array error: {}
            let array_type = self.parse_type(tree)?;
            let type_result = if self.match_current_tok(KfTokKind::SymSemi) {
                // this is an array definition
                let expr = self.parse_expression(tree)?;
                Ok(ast::TypeDecl {
                    reference_stack,
                    scope_res_relative,
                    scope_resolution,
                    type_id: ast::TypeKind::Array(Box::new(array_type), Some(expr)),
                })
            } else {
                Ok(ast::TypeDecl {
                    reference_stack,
                    scope_res_relative,
                    scope_resolution,
                    type_id: ast::TypeKind::Slice(Box::new(array_type)),
                })
            };
            self.consume_tok(KfTokKind::SymBracketClose)?;
            return type_result;
        } else {
            // parse_typename inlined here
            // ::foo?
            if self.match_current_tok(KfTokKind::SymScopeResolution) {
                scope_res_relative = false;
            }
            // foo::bar?
            while self.check_tok_sequence(&[KfTokKind::Literal, KfTokKind::SymScopeResolution]) {
                scope_resolution.push(self.consume_name_literal()?);
                self.consume_tok(KfTokKind::SymScopeResolution)?;
            }
            // bar<T, Z, K>?
            let (literal, generic_types) = self.parse_typename_with_generics(tree)?;
            if generic_types.len() > 0 {
                Ok(ast::TypeDecl {
                    reference_stack,
                    scope_res_relative,
                    scope_resolution,
                    type_id: ast::TypeKind::JustNameGeneric(literal, generic_types),
                })
            } else {
                Ok(ast::TypeDecl {
                    reference_stack,
                    scope_res_relative,
                    scope_resolution,
                    type_id: ast::TypeKind::JustName(literal),
                })
            }
        }
    }

    fn parse_typename_with_generics(&mut self, tree: &mut ast::Tree) -> ParsingResult<(String, Vec<ast::TypeDecl>)> {
        let name = self.consume_name_literal()?;
        let mut generic_types = Vec::new();
        if self.match_current_tok(KfTokKind::OpLt) {
            // parse_type_list inlined here
            while !self.match_current_tok(KfTokKind::OpGt) {
                generic_types.push(self.parse_type(tree)?);
                self.match_current_tok(KfTokKind::SymComma);
            }
        }
        Ok((name, generic_types))
    }

    fn parse_struct(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        self.consume_tok(KfTokKind::KwStruct)?;
        let at = self.get_curr()?.at;
        let (name, generic_args) = self.parse_typename_with_generics(tree)?;
        self.consume_tok(KfTokKind::SymCurlyOpen)?;
        let members = self.parse_struct_fields(tree)?;
        self.consume_tok(KfTokKind::SymCurlyClose)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::Struct(ast::Struct {
                name,
                generic_args,
                members,
            }),
            at,
        )))
    }

    fn parse_struct_fields(&mut self, tree: &mut ast::Tree) -> ParsingResult<Vec<ast::VarDecl>> {
        let mut ret = Vec::new();
        while self.get_curr()?.kind != KfTokKind::SymCurlyClose {
            let name = self.consume_name_literal()?;
            self.consume_tok(KfTokKind::SymColon)?;
            let type_dec = self.parse_type(tree)?;
            self.match_current_tok(KfTokKind::SymComma);
            ret.push(ast::VarDecl { name, type_dec })
        }
        Ok(ret)
    }

    fn parse_enum(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        self.consume_tok(KfTokKind::KwEnum)?;
        let at = self.get_curr()?.at;
        let (name, generic_args) = self.parse_typename_with_generics(tree)?;
        self.consume_tok(KfTokKind::SymCurlyOpen)?;
        let enums = self.parse_enum_fields(tree)?;
        self.consume_tok(KfTokKind::SymCurlyClose)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::Enum(ast::Enum {
                name,
                generic_args,
                enums,
            }),
            at,
        )))
    }

    fn parse_enum_fields(&mut self, tree: &mut ast::Tree) -> ParsingResult<Vec<(String, Option<ast::TypeDecl>)>> {
        let mut ret = Vec::new();
        while self.get_curr()?.kind != KfTokKind::SymCurlyClose {
            let name = self.consume_name_literal()?;
            // self.consume_tok(KfTokKind::SymColon)?;
            if self.match_current_tok(KfTokKind::SymParenthOpen) {
                let type_dec = self.parse_type(tree)?;
                ret.push((name, Some(type_dec)));
                self.consume_tok(KfTokKind::SymParenthClose)?;
            } else {
                ret.push((name, None));
            }
            self.match_current_tok(KfTokKind::SymComma);
        }
        Ok(ret)
    }

    fn parse_impl(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        self.consume_tok(KfTokKind::KwImpl)?;
        let at = self.get_curr()?.at;
        let name = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::SymCurlyOpen)?;
        let mut scope = Vec::new();
        while self.get_curr()?.kind != KfTokKind::SymCurlyClose {
            scope.push(self.parse_fun(tree)?);
        }
        self.consume_tok(KfTokKind::SymCurlyClose)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::Impl(ast::Impl { name, scope }),
            at,
        )))
    }

    fn parse_scope(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::SymCurlyOpen)?;
        let mut scope = Vec::new();
        while !self.check_current_tok(KfTokKind::SymCurlyClose) {
            if let Some(t) = self.iter.lookahead(1) {
                if t.kind == KfTokKind::OpAssign {
                    scope.push(self.parse_assign(tree)?);
                    continue;
                }
            }
            match self.get_curr()?.kind {
                KfTokKind::KwIf | KfTokKind::KwFor | KfTokKind::KwWhile | KfTokKind::KwLoop => {
                    scope.push(self.parse_ctrl_flow(tree)?)
                }
                KfTokKind::KwLet => scope.push(self.parse_var(tree)?),
                KfTokKind::KwBreak => {
                    let at = self.consume_tok(KfTokKind::KwBreak)?;
                    self.consume_tok(KfTokKind::SymSemi)?;
                    scope.push(tree.push(ast::Node::new(ast::Ntype::Break, at)));
                }
                KfTokKind::KwContinue => {
                    let at = self.consume_tok(KfTokKind::KwContinue)?;
                    self.consume_tok(KfTokKind::SymSemi)?;
                    scope.push(tree.push(ast::Node::new(ast::Ntype::Continue, at)));
                }
                _ => {
                    scope.push(self.parse_expression(tree)?);
                    self.consume_tok(KfTokKind::SymSemi)?;
                }
            }
        }
        self.consume_tok(KfTokKind::SymCurlyClose)?;
        Ok(tree.push(ast::Node::new(ast::Ntype::Scope(scope), at)))
    }

    fn parse_expression(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        Ok(self.parse_equality(tree)?)
    }

    fn parse_equality(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let mut expr = self.parse_comparison(tree)?;
        loop {
            let at = self.get_curr()?.at;
            if self.match_current_tok(KfTokKind::OpEq) {
                let right = self.parse_comparison(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Eq(expr, right), at));
                continue;
            } else if self.match_current_tok(KfTokKind::OpNe) {
                let right = self.parse_comparison(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Neq(expr, right), at));
                continue;
            }
            break;
        }
        Ok(expr)
    }

    fn parse_comparison(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let mut expr = self.parse_term(tree)?;
        loop {
            let at = self.get_curr()?.at;
            if self.match_current_tok(KfTokKind::OpGt) {
                let right = self.parse_term(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Gt(expr, right), at));
            } else if self.match_current_tok(KfTokKind::OpGe) {
                let right = self.parse_term(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Ge(expr, right), at));
            } else if self.match_current_tok(KfTokKind::OpLt) {
                let right = self.parse_term(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Lt(expr, right), at));
            } else if self.match_current_tok(KfTokKind::OpLe) {
                let right = self.parse_term(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Le(expr, right), at));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_term(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let mut expr = self.parse_factor(tree)?;
        loop {
            let at = self.get_curr()?.at;
            if self.match_current_tok(KfTokKind::OpPlus) {
                let right = self.parse_factor(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Plus(expr, right), at));
            } else if self.match_current_tok(KfTokKind::OpMinus) {
                let right = self.parse_factor(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Minus(expr, right), at));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_factor(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let mut expr = self.parse_unary(tree)?;
        loop {
            let at = self.get_curr()?.at;
            if self.match_current_tok(KfTokKind::OpSlash) {
                let right = self.parse_unary(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Slash(expr, right), at));
            } else if self.match_current_tok(KfTokKind::OpStar) {
                let right = self.parse_unary(tree)?;
                expr = tree.push(ast::Node::new(ast::Ntype::Star(expr, right), at));
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_unary(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.get_curr()?.at;
        if self.match_current_tok(KfTokKind::OpBang) {
            let expr = self.parse_unary(tree)?;
            return Ok(tree.push(ast::Node::new(ast::Ntype::Bang(expr), at)));
        } else if self.match_current_tok(KfTokKind::OpMinus) {
            let expr = self.parse_unary(tree)?;
            return Ok(tree.push(ast::Node::new(ast::Ntype::UMinus(expr), at)));
        }
        self.parse_primary(tree)
    }

    fn parse_primary(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let t = self.get_curr()?;
        if self.match_current_tok(KfTokKind::SlTrue) {
            return Ok(tree.push(ast::Node::new(ast::Ntype::True, t.at)));
        } else if self.match_current_tok(KfTokKind::SlFalse) {
            return Ok(tree.push(ast::Node::new(ast::Ntype::False, t.at)));
        } else if self.check_tok_sequence(&[KfTokKind::Literal, KfTokKind::SymParenthOpen]) {
            // this is a function call
            return self.parse_fn_call(tree);
        } else if self.match_current_tok(KfTokKind::LitChar) {
            return Ok(tree.push(ast::Node::new(ast::Ntype::Char(t.text.clone()), t.at)));
        } else if self.match_current_tok(KfTokKind::LitString) {
            return Ok(tree.push(ast::Node::new(ast::Ntype::String(t.text.clone()), t.at)));
        } else if self.match_current_tok(KfTokKind::Literal) {
            return Ok(tree.push(ast::Node::new(ast::Ntype::Literal(t.text.clone()), t.at)));
        } else if self.match_current_tok(KfTokKind::SymParenthOpen) {
            let expr = self.parse_expression(tree)?;
            self.consume_tok(KfTokKind::SymParenthClose)?;
            return Ok(expr);
        }
        Err(vec![comp_msg::error_invalid_primary_expression(t.at)])
    }

    fn parse_fn_call(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let fn_name = self.consume_name_literal()?;
        let at = self.consume_tok(KfTokKind::SymParenthOpen)?;
        let args = self.parse_expr_list(tree)?;
        self.consume_tok(KfTokKind::SymParenthClose)?;
        return Ok(tree.push(ast::Node::new(ast::Ntype::FnCall(fn_name, args), at)));
    }

    fn parse_expr_list(&mut self, tree: &mut ast::Tree ) -> ParsingResult<Vec<ast::NodeRef>> {
        let mut ret = Vec::new();
        // expression list can be empty
        if !self.check_current_tok(KfTokKind::SymParenthClose) {
            ret.push(self.parse_expression(tree)?);
        }
        while !self.check_current_tok(KfTokKind::SymParenthClose) {
            if self.match_current_tok(KfTokKind::SymComma) {
                ret.push(self.parse_expression(tree)?);
            } else {
                return Err(vec![comp_msg::error_expr_list_expected_coma_or_parenth(
                    self.get_curr()?.at,
                )]);
            }
        }
        Ok(ret)
    }

    fn parse_ctrl_flow(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        match self.get_curr()?.kind {
            KfTokKind::KwIf => self.parse_if(tree),
            KfTokKind::KwFor => self.parse_for(tree),
            KfTokKind::KwWhile => self.parse_while(tree),
            KfTokKind::KwLoop => self.parse_loop(tree),
            _ => Err(vec![comp_msg::error_expected_ctrl_flow(
                self.get_curr()?.at,
            )]),
        }
    }

    fn parse_if(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwIf)?;
        let cond = self.parse_expression(tree)?;
        let body = self.parse_scope(tree)?;
        let mut elif = Vec::new();
        while self.check_tok_sequence(&[KfTokKind::KwElse, KfTokKind::KwIf]) {
            self.consume_tok(KfTokKind::KwElse)?;
            self.consume_tok(KfTokKind::KwIf)?;
            let cond = self.parse_expression(tree)?;
            let body = self.parse_scope(tree)?;
            elif.push(ast::Elif { cond, body });
        }
        let else_body = if self.match_current_tok(KfTokKind::KwElse) {
            Some(self.parse_scope(tree)?)
        } else {
            None
        };
        Ok(tree.push(ast::Node::new(
            ast::Ntype::If(ast::If {
                cond,
                body,
                elif,
                else_body,
            }),
            at,
        )))
    }

    fn parse_for(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwFor)?;
        let var_pattern = self.consume_name_literal()?;
        self.consume_tok(KfTokKind::KwIn)?;
        let in_expr = self.parse_expression(tree)?;
        let body = self.parse_scope(tree)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::For(ast::For {
                var_pattern,
                in_expr,
                body,
            }),
            at,
        )))
    }

    fn parse_while(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwWhile)?;
        let cond = self.parse_expression(tree)?;
        let body = self.parse_scope(tree)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::While(ast::While { cond, body }),
            at,
        )))
    }

    fn parse_loop(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwLoop)?;
        let body = self.parse_scope(tree)?;
        Ok(tree.push(ast::Node::new(ast::Ntype::Loop(ast::Loop { body }), at)))
    }

    fn parse_var(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        if self.check_tok_sequence(&[KfTokKind::KwLet, KfTokKind::KwMut]) {
            self.parse_mut_var(tree)
        } else {
            self.parse_const_var(tree)
        }
    }

    fn parse_const_var(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwLet)?;
        let name = self.consume_name_literal()?;
        let vartype = if self.match_current_tok(KfTokKind::SymColon) {
            // Some(ast::TypeDecl::new(self.consume_name_literal()?))
            Some(self.parse_type(tree)?)
        } else {
            None
        };
        let expr = if self.match_current_tok(KfTokKind::OpAssign) {
            Some(self.parse_expression(tree)?)
        } else {
            None
        };
        self.consume_tok(KfTokKind::SymSemi)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::VarDef(ast::VarDef {
                mutable: false,
                name,
                vartype,
                expr,
            }),
            at,
        )))
    }

    fn parse_mut_var(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let at = self.consume_tok(KfTokKind::KwLet)?;
        self.consume_tok(KfTokKind::KwMut)?;
        let name = self.consume_name_literal()?;
        let vartype = if self.match_current_tok(KfTokKind::SymColon) {
            Some(ast::TypeDecl::new(self.consume_name_literal()?))
        } else {
            None
        };
        let expr = if self.match_current_tok(KfTokKind::OpAssign) {
            Some(self.parse_expression(tree)?)
        } else {
            None
        };
        self.consume_tok(KfTokKind::SymSemi)?;
        Ok(tree.push(ast::Node::new(
            ast::Ntype::VarDef(ast::VarDef {
                mutable: true,
                name,
                vartype,
                expr,
            }),
            at,
        )))
    }

    fn parse_assign(&mut self, tree: &mut ast::Tree) -> ParsingResult<ast::NodeRef> {
        let name = self.consume_name_literal()?;
        let at = self.consume_tok(KfTokKind::OpAssign)?;
        let expr = self.parse_expression(tree)?;
        self.consume_tok(KfTokKind::SymSemi)?;
        Ok(tree.push(ast::Node::new(ast::Ntype::Assign(name, expr), at)))
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
    fn consume_tok(&mut self, kind: KfTokKind) -> ParsingResult<TextPoint> {
        let t = self.iter.peek();
        match t {
            Some(t) => {
                if t.kind == kind {
                    let at = t.at;
                    self.iter.next();
                    return Ok(at);
                } else {
                    return Err(vec![comp_msg::error_unexpected_token(&kind, &t)]);
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
                    return Err(vec![comp_msg::error_expected_literal(&t)]);
                }
            }
            None => return Err(error_eof()),
        }
    }
}

fn error_eof() -> comp_msg::CompileMsgCol {
    vec![comp_msg::error_eof_generic()]
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
                let at = e.at.unwrap_or(TextPoint { line: 0, col: 0 });
                let msg = format!("Syntax error at {}, {}: {}", at.line, at.col, e.msg);
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
                    let at = e.at.unwrap_or(TextPoint { line: 1, col: 1 });
                    err_msg += code.split("\n").collect::<Vec<_>>()[at.line - 1];
                    err_msg += &format!("\n{}^\n", " ".repeat(at.col - 1));
                    err_msg += &format!("Syntax error: {}\n", e.msg);
                    err_msg += &format!("Details: {}\n", e.details);
                    // err_msg += &format!("Tokens: {:?}", tokens);
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
        // these are the correct Kulfon code snippets
        // from the parser perspective! Type checking and
        // other advanced stuff don't happen here.
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
            "let a: &i32; let b: &&bool; let d: &mutf32;",
            "let a: [f32]; let b: [i16; 10+5]; let c: [myStruct; _]; let d: &[s;1];",
            "let a: ::size_t; let b: Point<f32>; let c: my_mod::TypeT<a, b, ::size_t>;",
            "struct MyStruct {} struct A {x: i32} struct b {y:i32, z:bool}",
            "enum T {A, B, C} enum B {A, B(i32), C(MyStruct)}",
            "impl A { fn B(){} }",
            "fn a(i:i32,...) {}",
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

    #[test]
    fn parse_built_in_stuff() {
        use crate::compiler::BUILT_IN_STUFF;
        match parse_code(BUILT_IN_STUFF) {
            Ok(_) => {}
            Err(e) => {
                // println!("Parsing built-in code:\n{}", BUILT_IN_STUFF);
                println!("{e}");
                assert!(false);
            }
        }
    }
}
