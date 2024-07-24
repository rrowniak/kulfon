// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.06.2024
// ---------------------------------------------------

use crate::lang_def::{Lang, RangeBased};
use crate::comp_msg::{TextPoint, CompileMsgCol};
use crate::comp_msg;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Keyword,
    Literal,
    Symbol,
    String,
    Character,
    Comment,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Keyword => write!(f, "keyword"),
            TokenKind::Literal => write!(f, "literal"),
            TokenKind::Symbol => write!(f, "symbol"),
            TokenKind::String => write!(f, "string"),
            TokenKind::Character => write!(f, "char"),
            TokenKind::Comment => write!(f, "comment"),
        }
    }
}

pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub start: TextPoint,
    pub end: TextPoint,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        self.kind.fmt(f)?;
        write!(f, ": '")?;
        self.text.fmt(f)?;
        write!(f, "']")
    }
}
pub fn tokenize(lang: &Lang, code: &str) -> (Vec<Token>, CompileMsgCol) {
    Tokenizer::new(lang, code).tokenize()
}

struct Tokenizer<'a> {
    lang: &'a Lang,
    code: &'a str,
    curr_point: TextPoint,
    skip_n: usize,
    pos: usize,
    iter: std::str::CharIndices<'a>,
    prev_c: char,
    repeat_this_pass: bool,
}

#[derive(Clone)]
enum TokenizeState {
    Idle,
    ParseLiteral,
    ParseRange(RangeBased),
}

impl<'a> Tokenizer<'a> {
    fn new(lang: &'a Lang, code: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            lang,
            code,
            curr_point: TextPoint { line: 1, col: 1 },
            skip_n: 0,
            pos: 0,
            iter: code.char_indices(),
            prev_c: '\0',
            repeat_this_pass: false,
        }
    }

    fn update_prev_c(&mut self) {
        if self.prev_c == '\n' {
            self.curr_point.col = 1;
            self.curr_point.line += 1;
        } else {
            self.curr_point.col += 1;
        }
    }

    fn next(&mut self) -> Option<char> {
        if self.repeat_this_pass {
            self.repeat_this_pass = false;
            return Some(self.prev_c);
        }
        while self.skip_n > 0 {
            self.update_prev_c();
            if let Some((_, c)) = self.iter.next() {
                self.prev_c = c;
            }
            self.skip_n -= 1;
        }

        if let Some((pos, c)) = self.iter.next() {
            self.update_prev_c();
            self.prev_c = c;
            self.pos = pos;
            Some(c)
        } else {
            None
        }
    }

    fn tokenize(&mut self) -> (Vec<Token>, CompileMsgCol) {
        let mut tokens: Vec<Token> = Vec::new();
        let mut errors  = CompileMsgCol::new(); 

        let mut start_point = TextPoint { line: 1, col: 1 };
        let mut start_pos = 0;
        let mut status = TokenizeState::Idle;

        while let Some(c) = self.next() {
            match status.clone() {
                TokenizeState::Idle => {
                    // skip on whitespace
                    if c.is_ascii_whitespace() {
                        continue;
                    }
                    if !c.is_ascii() {
                        // non-ascii character not expected here
                        errors.push(comp_msg::error_non_ascii(self.curr_point));
                        continue;
                    }
                    let mut found = false;
                    // range based token?
                    for r in self.lang.range_based.iter() {
                        if self.look_ahead(&r.get_range().start) {
                            // start parsing this token
                            status = TokenizeState::ParseRange(r.clone());
                            // -1 because we don't need to skip the current character
                            // it's already "skipped"
                            self.skip_n = r.get_range().start.len() - 1;
                            start_point = self.curr_point;
                            start_pos = self.pos + r.get_range().start.len();
                            found = true;
                            break;
                        }
                    }
                    if found {
                        continue;
                    }
                    // try parsing characters
                    // this is a special case - if Lang::special_sym contains >'< then
                    // we won't emit any errors if parsing fails
                    if c == '\'' {
                        // this might be:
                        // 1. simple character like 'a'
                        // 2. escapte sequence like '\n'
                        // 3. non character at all if >'< is a special character (e.g 'lifecycle)
                        // 'a'
                        //   ^-- offset = 2
                        // ^-- self.pos
                        if let Some(cc) = self.get_nth(2) {
                            if cc == '\'' {
                                self.skip_n = 2;
                                tokens.push(Token {
                                    kind: TokenKind::Character,
                                    text: self.code[self.pos + 1..self.pos + 2].to_string(),
                                    start: self.curr_point,
                                    end: TextPoint {
                                        line: self.curr_point.line,
                                        col: self.curr_point.col + 2,
                                    },
                                });
                                continue;
                            }
                        } else {
                            // There is one edge case which is not handled here: 'a<EOF>.
                            // But this is a theoretical case which might be relevant to Rust.
                            // Kulfon lang shouldn't be affected
                            errors.push(comp_msg::error_eof_unterminated_char());
                            continue;
                        }

                        // '\n'
                        //  ^----- offset 1
                        //    ^----- offset 3
                        if let Some(cc) = self.get_nth(1) {
                            if cc == '\\' {
                                if let Some(ce) = self.get_nth(3) {
                                    if ce == '\'' {
                                        self.skip_n = 3;
                                        tokens.push(Token {
                                            kind: TokenKind::Character,
                                            text: self.code[self.pos + 1..self.pos + 3].to_string(),
                                            start: self.curr_point,
                                            end: TextPoint {
                                                line: self.curr_point.line,
                                                col: self.curr_point.col + 3,
                                            },
                                        });
                                        continue;
                                    }
                                } else {
                                    errors
                                        .push(comp_msg::error_eof_unterminated_char());
                                }
                            }
                        }
                        // if we're here, there are two options: >'< is a symbol or we have
                        // parsing error
                        if !self.lang.special_sym.contains(&"'".to_string()) {
                            errors.push(comp_msg::error_invalid_char(self.curr_point));
                        }
                    }
                    // special symbol?
                    for s in self.lang.special_sym.iter() {
                        if self.look_ahead(&s) {
                            // we need to be careful with >_< (underscore) because:
                            // `call(_)` - this is symbol,
                            // `_variable` - this is literal
                            // `__` - this is literal (another corner case!)
                            if let Some(cc) = self.get_nth(1) {
                                if (s == "_") && (cc.is_ascii_alphanumeric() || cc == '_') {
                                    // next character is alphanumeric - this is literal case
                                    break;
                                }
                            }
                            // insert token and move ahead!
                            // we can do that because special symbol can't cross a line
                            self.skip_n = s.len() - 1;
                            tokens.push(Token {
                                kind: TokenKind::Symbol,
                                text: self.code[self.pos..self.pos + s.len()].to_string(),
                                start: self.curr_point,
                                end: TextPoint {
                                    line: self.curr_point.line,
                                    col: self.curr_point.col + self.skip_n,
                                },
                            });
                            found = true;
                            status = TokenizeState::Idle;
                            break;
                        }
                    }
                    if found {
                        continue;
                    }
                    // it must be literal then...
                    status = TokenizeState::ParseLiteral;
                    start_point = self.curr_point;
                    start_pos = self.pos;
                    continue;
                }
                TokenizeState::ParseLiteral => {
                    // check finish conditions
                    if !(c.is_ascii_alphanumeric() || c == '_') {
                        status = TokenizeState::Idle;
                        tokens.push(self.gen_token_literal_or_keyword(start_pos, start_point));
                        self.repeat_this_pass = true;
                        continue;
                    }
                }
                TokenizeState::ParseRange(r) => {
                    let mut skip_this_pass = false;
                    for ex in r.get_range().exceptions.iter() {
                        if self.look_ahead(&ex) {
                            self.skip_n = ex.len() - 1;
                            skip_this_pass = true;
                            break;
                        }
                    }
                    if skip_this_pass {
                        continue;
                    }
                    if self.look_ahead(&r.get_range().end) {
                        status = TokenizeState::Idle;
                        let kind = match r {
                            RangeBased::LineComment(_)
                            // | RangeBased::DocComment(_)
                            | RangeBased::Comment(_) => TokenKind::Comment,
                            RangeBased::String(_) | RangeBased::RawString(_) => TokenKind::String,
                        };
                        tokens.push(Token {
                            kind,
                            text: self.code[start_pos..self.pos].to_string(),
                            start: start_point,
                            end: self.curr_point,
                        });
                        self.skip_n = r.get_range().end.len() - 1;
                        continue;
                    }
                }
            }
        }
        // handle eof case
        match status {
            TokenizeState::Idle => {}
            TokenizeState::ParseLiteral => {
                // artificial increment pos to capture the last variable
                self.pos += 1;
                tokens.push(self.gen_token_literal_or_keyword(start_pos, start_point));
            }
            TokenizeState::ParseRange(r) => {
                if r.get_range().eof_allowed {
                    let kind = match r {
                        RangeBased::LineComment(_)
                        // | RangeBased::DocComment(_)
                        | RangeBased::Comment(_) => TokenKind::Comment,
                        RangeBased::String(_) | RangeBased::RawString(_) => TokenKind::String,
                    };
                    tokens.push(Token {
                        kind,
                        text: self.code[start_pos..].to_string(),
                        start: start_point,
                        end: self.curr_point,
                    });
                } else {
                    errors.push(comp_msg::error_eof_unterminated_string(self.curr_point));
                }
            }
        }
        (tokens, errors)
    }

    fn look_ahead(&self, phrase: &str) -> bool {
        if let Some(code) = self.code.get(self.pos..) {
            code.starts_with(phrase)
        } else {
            return false;
        }
    }

    fn get_nth(&self, offset: usize) -> Option<char> {
        if let Some(nth) = self.code.get(offset..offset + 1) {
            nth.chars().next()
        } else {
            None
        }
    }

    fn gen_token_literal_or_keyword(&self, start_pos: usize, start_point: TextPoint) -> Token {
        let text = self.code[start_pos..self.pos].to_string();
        let kind = if self.lang.keywords.contains(&text) {
            TokenKind::Keyword
        } else {
            TokenKind::Literal
        };

        Token {
            kind,
            text,
            start: start_point,
            end: self.curr_point,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_empty_result() {
        let tcs = ["", " ", "\t ", "\t"];
        for code in tcs {
            let (t, e) = tokenize(&Lang::new(), code);
            assert_eq!(t.len(), 0);
            assert_eq!(e.len(), 0);
        }
    }

    #[test]
    fn one_token_test() {
        #[rustfmt::skip]
        let tcs = [
            // keywords
            ("fn\n", "fn", TokenKind::Keyword),
            ("else ", "else", TokenKind::Keyword),
            ("for", "for", TokenKind::Keyword),
            // comments
            ("// comment\n", " comment", TokenKind::Comment),
            ("// comment\n\n", " comment", TokenKind::Comment),
            ("//comment ółń", "comment ółń", TokenKind::Comment),
            ("//comment //abc", "comment //abc", TokenKind::Comment),
            ("/**/\n", "", TokenKind::Comment),
            ("/* */", " ", TokenKind::Comment),
            ("/** abc **/", "* abc *", TokenKind::Comment),
            // strings
            ("r#\"\"#", "", TokenKind::String),
            ("r#\"_\\\"_\"#", "_\\\"_", TokenKind::String),
            ("r#\"string\"#", "string", TokenKind::String),
            ("\"string\"", "string", TokenKind::String),
            ("\"string\\\"\"", "string\\\"", TokenKind::String),
            // characters
            ("'a'", "a", TokenKind::Character),
            ("'_'", "_", TokenKind::Character),
            ("'\\n'", "\\n", TokenKind::Character),
            // symbols
            ("++", "++", TokenKind::Symbol),
            ("+\t", "+", TokenKind::Symbol),
            ("!", "!", TokenKind::Symbol),
            ("#[ ", "#[", TokenKind::Symbol),
            ("<=", "<=", TokenKind::Symbol),
            (" <=", "<=", TokenKind::Symbol),
            ("=\n", "=", TokenKind::Symbol),
            ("_", "_", TokenKind::Symbol),
            // literals
            ("1920", "1920", TokenKind::Literal),
            ("_beta", "_beta", TokenKind::Literal),
            ("__", "__", TokenKind::Literal),
            (" _zeta_", "_zeta_", TokenKind::Literal),
            ("a_b_c", "a_b_c", TokenKind::Literal),
            ("CamelCase\t\t", "CamelCase", TokenKind::Literal),
            ("Kulfon", "Kulfon", TokenKind::Literal),
            ("\tmac091cc\t\t", "mac091cc", TokenKind::Literal),
        ];
        for code in tcs {
            println!("Testing {}", code.0);
            let (t, e) = tokenize(&Lang::new(), code.0);
            for token in t.iter() {
                println!("Result: {token}");
            }
            for err in e.iter() {
                println!("Error: {}", err.msg);
            }
            assert_eq!(t.len(), 1);
            assert_eq!(e.len(), 0);
            assert_eq!(t[0].text, code.1);
            assert_eq!(t[0].kind, code.2);
        }
    }

    #[test]
    fn test_token_seq() {
        let tcs = [
            ("+variable", vec![(TokenKind::Symbol, "+"), (TokenKind::Literal, "variable")]),
        ];

        for tc in tcs {
            println!("Testing {}...", tc.0);
            let (t, e) = tokenize(&Lang::new(), tc.0);
            
            for token in t.iter() {
                println!("Result: {token}");
            }
            for err in e.iter() {
                println!("Error: {}", err.msg);
            }
            assert_eq!(e.len(), 0);
            assert_eq!(t.len(), tc.1.len());
            for (t, exp) in t.iter().zip(tc.1) {
                assert_eq!(t.kind, exp.0);
                assert_eq!(t.text, exp.1);
            }
        }
    }
}
