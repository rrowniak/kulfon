use crate::lang_def::{Lang, ParsingError, Range, RangeBased, TextPoint};
use std::fmt;

pub enum TokenKind {
    Keyword,
    // Identifier,
    Literal,
    Symbol,
    // Operator,
    // Separator,
    // Number,
    String,
    Character,
    Comment,
    // NewLine,
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
    kind: TokenKind,
    text: String,
    start: TextPoint,
    end: TextPoint,
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
pub fn tokenize(lang: &Lang, code: &str) -> (Vec<Token>, Vec<ParsingError>) {
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
            curr_point: TextPoint { line: 0, col: 0 },
            skip_n: 0,
            pos: 0,
            iter: code.char_indices(),
            prev_c: '\0',
            repeat_this_pass: false,
        }
    }

    fn update_prev_c(&mut self) {
        if self.prev_c == '\n' {
            self.curr_point.col = 0;
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

    fn tokenize(&mut self) -> (Vec<Token>, Vec<ParsingError>) {
        let mut tokens: Vec<Token> = Vec::new();
        let mut errors: Vec<ParsingError> = Vec::new();

        let mut start_point = TextPoint { line: 0, col: 0 };
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
                        errors.push(self.error_non_ascii());
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
                            errors.push(self.error_eof("Character literal should be closed"));
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
                                        tokens.push(Token{
                                            kind: TokenKind::Character,
                                            text: self.code[self.pos + 1..self.pos + 3].to_string(),
                                            start: self.curr_point,
                                            end: TextPoint {
                                                line: self.curr_point.line,
                                                col: self.curr_point.col + 3,
                                            },
                                        });
                                        continue
                                    }
                                } else {
                                    errors.push(self.error_eof("Character literal should be closed"));
                                }
                            }
                        } 
                        // if we're here, there are two options: >'< is a symbol or we have
                        // parsing error
                        if !self.lang.special_sym.contains(&"'".to_string()) {
                            errors.push(ParsingError{
                                msg: "Syntax error while parsing character literal".into(),
                                details: "Expected formats either like 'a' or '\n'".into(),
                                at: self.curr_point,
                            })
                        }
                    }
                    // special symbol?
                    for s in self.lang.special_sym.iter() {
                        if self.look_ahead(&s) {
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
                    if !(c.is_ascii_alphanumeric()) {
                        status = TokenizeState::Idle;
                        tokens.push(Token {
                            kind: TokenKind::Literal,
                            text: self.code[start_pos..self.pos].to_string(),
                            start: start_point,
                            end: self.curr_point,
                        });
                        self.repeat_this_pass = true;
                        continue;
                    }
                }
                TokenizeState::ParseRange(r) => {
                    let mut skip_this_pass = false;
                    for ex in r.get_range().exceptions.iter() {
                        if self.look_ahead(&ex) {
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
                            | RangeBased::DocComment(_)
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
                tokens.push(Token {
                    kind: TokenKind::Literal,
                    text: self.code[start_pos..self.pos].to_string(),
                    start: start_point,
                    end: self.curr_point,
                });
            }
            TokenizeState::ParseRange(r) => {
                if r.get_range().eof_allowed {
                    let kind = match r {
                        RangeBased::LineComment(_)
                        | RangeBased::DocComment(_)
                        | RangeBased::Comment(_) => TokenKind::Comment,
                        RangeBased::String(_) | RangeBased::RawString(_) => TokenKind::String,
                    };
                    tokens.push(Token {
                        kind,
                        text: self.code[start_pos..self.pos].to_string(),
                        start: start_point,
                        end: self.curr_point,
                    });
                } else {
                    errors.push(ParsingError {
                        msg: "Unexpected end of file".to_string(),
                        details: "String or comment needs to be terminated".to_string(),
                        at: self.curr_point,
                    });
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

    fn error_non_ascii(&self) -> ParsingError {
        ParsingError {
            msg: "Unexpected non-ASCII character".to_string(),
            details: "Non-ASCII characters allowed in strings or comments only".to_string(),
            at: self.curr_point,
        }
    }

    fn error_eof(&self, details: &str) -> ParsingError {
        ParsingError {
            msg: "Unexpected end of file".to_string(),
            details: details.to_string(),
            at: self.curr_point,
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
            "fn\n", "// comment\n", "/**/\n", "r#\"\"#", "r#\"string\"#",
            // characters
            "'a'", "'\\n'",
            // symbols
            "++", "+", "!", "#[", "<=", "=",
        ];
        for code in tcs {
            println!("Testing {code}");
            let (t, e) = tokenize(&Lang::new(), code);
            for token in t.iter() {
                println!("Result: {token}");
            }
            for err in e.iter() {
                println!("Error: {}", err.msg);
            }
            assert_eq!(t.len(), 1);
            assert_eq!(e.len(), 0);
        }
    }
}
