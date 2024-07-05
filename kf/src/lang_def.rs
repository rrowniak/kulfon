// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.06.2024
// ---------------------------------------------------

#[rustfmt::skip]
const RUST_KEYWORDS: &[&str] = &[
    // Keywords used in the language
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
    "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
    "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
    "use", "where", "while",

    // Keywords reserved for future use
    "abstract", "become", "box", "do", "final", "macro", "override", "priv", "try", "typeof",
    "unsized", "virtual", "yield",

    // Keywords that are not used in Rust, but are reserved for compatibility with other languages
    "alignof", "become", "offsetof", "priv", "pure", "sizeof", "typeof", "unsized", "yield"
];

const KULFON_KEYWORDS: &[&str] = &[
    KF_IF,
    KF_ELSE,
    KF_FOR,
    KF_WHILE,
    KF_LOOP,
    KF_FN,
    KF_PUB,
    KF_LET,
    KF_MUT,
    KF_IN,
    KF_BREAK,
    KF_CONTINUE,
];
const KULFON_RES_KEYWORDS: &[&str] = RUST_KEYWORDS;
#[rustfmt::skip]
const KULFON_SPEC_SYMBOLS: &[&str] = &[
    KF_CURLY_OPEN, KF_CURLY_CLOSE, KF_PARENTH_OPEN, KF_PARENTH_CLOSE, KF_SEMI, KF_COLON, KF_ARROW,
    KF_DOT, KF_COMMA, KF_EQ, KF_NE, KF_GT, KF_GE, KF_LT, KF_LE, KF_MINUS, KF_PLUS, KF_SLASH,
    KF_STAR, KF_BANG, KF_ASSIGN,
    // Single-character symbols
    "[", "]", "%", "^", "&", "|", "~",
    "@", "#", "$", "?", ";", ":", ",", ".", "'", "\"", "_",
    // Multi-character operators and symbols
    "=>", "::", "..", "...", "..=", "&&", "||", "<<", ">>", "+=", 
    "-=", "*=", "/=", "%=", "^=", "&=", "|=", "<<=", ">>=", "++", "--", 
    // Attribute and macro-related
    "#!", "#[", "]", "?"
];

// kulfon keywords
const KF_IF: &str = "if";
const KF_ELSE: &str = "else";
const KF_FOR: &str = "for";
const KF_WHILE: &str = "while";
const KF_LOOP: &str = "loop";
const KF_FN: &str = "fn";
const KF_PUB: &str = "pub";
const KF_LET: &str = "let";
const KF_MUT: &str = "mut";
const KF_IN: &str = "in";
const KF_BREAK: &str = "break";
const KF_CONTINUE: &str = "continue";
// kulfon special literals
const KF_TRUE: &str = "true";
const KF_FALSE: &str = "false";
// kulfon symbols
const KF_CURLY_OPEN: &str = "{";
const KF_CURLY_CLOSE: &str = "}";
const KF_PARENTH_OPEN: &str = "(";
const KF_PARENTH_CLOSE: &str = ")";
const KF_SEMI: &str = ";";
const KF_COLON: &str = ":";
const KF_ARROW: &str = "->";
const KF_DOT: &str = ".";
const KF_COMMA: &str = ",";
// operators
const KF_EQ: &str = "==";
const KF_NE: &str = "!=";
const KF_GT: &str = ">";
const KF_GE: &str = ">=";
const KF_LT: &str = "<";
const KF_LE: &str = "<=";
const KF_PLUS: &str = "+";
const KF_MINUS: &str = "-";
const KF_SLASH: &str = "/";
const KF_STAR: &str = "*";
const KF_BANG: &str = "!";
const KF_ASSIGN: &str = "=";

#[derive(Debug, PartialEq)]
pub enum KfTokKind {
    // keywords
    KwIf,
    KwElse,
    KwFor,
    KwWhile,
    KwLoop,
    KwFn,
    KwPub,
    KwLet,
    KwMut,
    KwIn,
    KwBreak,
    KwContinue,
    // special literals
    SlTrue,
    SlFalse,
    // symbols
    SymCurlyOpen,
    SymCurlyClose,
    SymParenthOpen,
    SymParenthClose,
    SymSemi,
    SymColon,
    SymArrow,
    SymDot,
    SymComma,
    // operators
    OpEq,
    OpNe,
    OpGt,
    OpGe,
    OpLt,
    OpLe,
    OpPlus,
    OpMinus,
    OpSlash,
    OpStar,
    OpBang,
    OpAssign,
    // literals
    LitString,
    LitChar,
    // LitInt,
    // LitFloat,
    Literal,
    Comment,
}

impl KfTokKind {
    pub fn from(s: &str) -> Option<KfTokKind> {
        let t = match s {
            KF_IF => KfTokKind::KwIf,
            KF_ELSE => KfTokKind::KwElse,
            KF_FOR => KfTokKind::KwFor,
            KF_WHILE => KfTokKind::KwWhile,
            KF_LOOP => KfTokKind::KwLoop,
            KF_FN => KfTokKind::KwFn,
            KF_PUB => KfTokKind::KwPub,
            KF_LET => KfTokKind::KwLet,
            KF_MUT => KfTokKind::KwMut,
            KF_IN => KfTokKind::KwIn,
            KF_BREAK => KfTokKind::KwBreak,
            KF_CONTINUE => KfTokKind::KwContinue,
            KF_TRUE => KfTokKind::SlTrue,
            KF_FALSE => KfTokKind::SlFalse,
            KF_CURLY_OPEN => KfTokKind::SymCurlyOpen,
            KF_CURLY_CLOSE => KfTokKind::SymCurlyClose,
            KF_PARENTH_OPEN => KfTokKind::SymParenthOpen,
            KF_PARENTH_CLOSE => KfTokKind::SymParenthClose,
            KF_SEMI => KfTokKind::SymSemi,
            KF_COLON => KfTokKind::SymColon,
            KF_ARROW => KfTokKind::SymArrow,
            KF_DOT => KfTokKind::SymDot,
            KF_COMMA => KfTokKind::SymComma,
            KF_EQ => KfTokKind::OpEq,
            KF_NE => KfTokKind::OpNe,
            KF_GT => KfTokKind::OpGt,
            KF_GE => KfTokKind::OpGe,
            KF_LT => KfTokKind::OpLt,
            KF_LE => KfTokKind::OpLe,
            KF_PLUS => KfTokKind::OpPlus,
            KF_MINUS => KfTokKind::OpMinus,
            KF_SLASH => KfTokKind::OpSlash,
            KF_STAR => KfTokKind::OpStar,
            KF_BANG => KfTokKind::OpBang,
            KF_ASSIGN => KfTokKind::OpAssign,
            _ => return None,
        };
        Some(t)
    }
}

#[derive(Debug)]
pub struct KfToken {
    pub kind: KfTokKind,
    pub text: String,
    pub at: TextPoint,
}

#[derive(Clone)]
pub struct Range {
    pub start: String,
    pub end: String,
    pub exceptions: Vec<String>,
    pub eof_allowed: bool,
}

#[derive(Clone)]
pub enum RangeBased {
    LineComment(Range),
    // DocComment(Range),
    Comment(Range),
    String(Range),
    RawString(Range),
}

impl RangeBased {
    pub fn get_range(&self) -> &Range {
        match self {
            RangeBased::LineComment(r) => r,
            // RangeBased::DocComment(r) => r,
            RangeBased::Comment(r) => r,
            RangeBased::String(r) => r,
            RangeBased::RawString(r) => r,
        }
    }
}

pub struct Lang {
    pub keywords: Vec<String>,
    pub reserved_keywords: Vec<String>,
    pub special_sym: Vec<String>,
    pub range_based: Vec<RangeBased>,
}

impl Lang {
    // pub fn new_empty() -> Lang {
    //     Lang {
    //         keywords: Vec::new(),
    //         reserved_keywords: Vec::new(),
    //         special_sym: Vec::new(),
    //         range_based: Vec::new(),
    //     }
    // }
    pub fn new() -> Lang {
        let mut sym = KULFON_SPEC_SYMBOLS
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();
        // sort by length starting from the longer symobol
        // that order is needed by lexer
        sym.sort_by(|a, b| b.len().cmp(&a.len()));

        Lang {
            keywords: KULFON_KEYWORDS.iter().map(|s| s.to_string()).collect(),
            reserved_keywords: KULFON_RES_KEYWORDS.iter().map(|s| s.to_string()).collect(),
            special_sym: sym,
            range_based: vec![
                RangeBased::LineComment(Range {
                    start: "//".into(),
                    end: "\n".into(),
                    exceptions: Vec::new(),
                    eof_allowed: true,
                }),
                RangeBased::Comment(Range {
                    start: "/*".into(),
                    end: "*/".into(),
                    exceptions: Vec::new(),
                    eof_allowed: false,
                }),
                RangeBased::String(Range {
                    start: "\"".into(),
                    end: "\"".into(),
                    exceptions: vec!["\\\"".into()],
                    eof_allowed: false,
                }),
                RangeBased::RawString(Range {
                    start: "r#\"".into(),
                    end: "\"#".into(),
                    exceptions: Vec::new(),
                    eof_allowed: false,
                }),
            ],
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct TextPoint {
    pub line: usize,
    pub col: usize,
}

pub struct ParsingError {
    pub msg: String,
    pub details: String,
    pub at: TextPoint,
}
