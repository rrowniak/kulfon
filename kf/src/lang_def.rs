// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.06.2024
// ---------------------------------------------------
use crate::comp_msg::TextPoint;

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
    KF_STRUCT,
    KF_ENUM,
    KF_IMPL,
    KF_SELF,
    KF_SELFT,
    KF_TRUE,
    KF_FALSE,
];
const KULFON_RES_KEYWORDS: &[&str] = RUST_KEYWORDS;
#[rustfmt::skip]
const KULFON_SPEC_SYMBOLS: &[&str] = &[
    KF_CURLY_OPEN, KF_CURLY_CLOSE, KF_PARENTH_OPEN, KF_PARENTH_CLOSE, 
    KF_BRACKET_OPEN, KF_BRACKET_CLOSE, KF_AMPERSAND, KF_SCOPE_RESOL, KF_AT,
    KF_SEMI, KF_COLON, KF_ARROW,
    KF_DOT, KF_COMMA, KF_EQ, KF_NE, KF_GT, KF_GE, KF_LT, KF_LE, KF_MINUS, KF_PLUS, KF_SLASH,
    KF_STAR, KF_BANG, KF_ASSIGN, KF_AND, KF_OR, KF_ELLIPSIS,
    // Single-character symbols
    "%", "^", "|", "~",
    "#", "$", "?", ";", ":", ",", ".", "'", "\"", "_",
    // Multi-character operators and symbols
    "=>", "..", "..=", "<<", ">>", "+=", 
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
const KF_STRUCT: &str = "struct";
const KF_ENUM: &str = "enum";
const KF_IMPL: &str = "impl";
const KF_SELF: &str = "self";
const KF_SELFT: &str = "Self";
// kulfon special literals
const KF_TRUE: &str = "true";
const KF_FALSE: &str = "false";
// kulfon symbols
const KF_CURLY_OPEN: &str = "{";
const KF_CURLY_CLOSE: &str = "}";
const KF_PARENTH_OPEN: &str = "(";
const KF_PARENTH_CLOSE: &str = ")";
const KF_BRACKET_OPEN: &str = "[";
const KF_BRACKET_CLOSE: &str = "]";
const KF_AMPERSAND: &str = "&";
const KF_SCOPE_RESOL: &str = "::";
const KF_AT: &str = "@";
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
const KF_AND: &str = "&&";
const KF_OR: &str = "||";
const KF_ELLIPSIS: &str = "...";

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
    KwStruct,
    KwEnum,
    KwImpl,
    KwSelf,
    KwSelfT,
    // special literals
    SlTrue,
    SlFalse,
    // symbols
    SymCurlyOpen,
    SymCurlyClose,
    SymParenthOpen,
    SymParenthClose,
    SymBracketOpen,
    SymBracketClose,
    SymAmpersand,
    SymScopeResolution,
    SymAt,
    SymSemi,
    SymColon,
    SymArrow,
    SymDot,
    SymComma,
    SymEllipsis,
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
    OpAnd,
    OpOr,
    // literals
    LitString,
    LitChar,
    // LitInt,
    // LitFloat,
    Literal,
    // Comment,
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
            KF_STRUCT => KfTokKind::KwStruct,
            KF_ENUM => KfTokKind::KwEnum,
            KF_IMPL => KfTokKind::KwImpl,
            KF_SELF => KfTokKind::KwSelf,
            KF_SELFT => KfTokKind::KwSelfT,
            KF_TRUE => KfTokKind::SlTrue,
            KF_FALSE => KfTokKind::SlFalse,
            KF_CURLY_OPEN => KfTokKind::SymCurlyOpen,
            KF_CURLY_CLOSE => KfTokKind::SymCurlyClose,
            KF_PARENTH_OPEN => KfTokKind::SymParenthOpen,
            KF_PARENTH_CLOSE => KfTokKind::SymParenthClose,
            KF_BRACKET_OPEN => KfTokKind::SymBracketOpen,
            KF_BRACKET_CLOSE =>KfTokKind::SymBracketClose,
            KF_AMPERSAND => KfTokKind::SymAmpersand,
            KF_SCOPE_RESOL => KfTokKind::SymScopeResolution,
            KF_AT => KfTokKind::SymAt,
            KF_SEMI => KfTokKind::SymSemi,
            KF_COLON => KfTokKind::SymColon,
            KF_ARROW => KfTokKind::SymArrow,
            KF_DOT => KfTokKind::SymDot,
            KF_COMMA => KfTokKind::SymComma,
            KF_ELLIPSIS => KfTokKind::SymEllipsis,
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
            KF_AND => KfTokKind::OpAnd,
            KF_OR => KfTokKind::OpOr,
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
pub struct Range<'a> {
    pub start: &'a str,
    pub end: &'a str,
    pub exceptions: Vec<&'a str>,
    pub eof_allowed: bool,
}

#[derive(Clone)]
pub enum RangeBased<'a> {
    LineComment(Range<'a>),
    // DocComment(Range),
    Comment(Range<'a>),
    String(Range<'a>),
    RawString(Range<'a>),
}

impl<'a> RangeBased<'a> {
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

pub struct Lang<'a> {
    pub keywords: Vec<&'a str>,
    pub reserved_keywords: Vec<&'a str>,
    pub special_sym: Vec<&'a str>,
    pub range_based: Vec<RangeBased<'a>>,
}

impl<'a> Lang<'a> {
        pub fn new() -> Lang<'a> {
        let mut sym = KULFON_SPEC_SYMBOLS.to_vec();
        // sort by length starting from the longer symobol
        // that order is needed by lexer
        sym.sort_by(|a, b| b.len().cmp(&a.len()));

        Lang {
            keywords: KULFON_KEYWORDS.to_vec(),
            reserved_keywords: KULFON_RES_KEYWORDS.to_vec(),
            special_sym: sym,
            range_based: vec![
                RangeBased::LineComment(Range {
                    start: "//",
                    end: "\n",
                    exceptions: Vec::new(),
                    eof_allowed: true,
                }),
                RangeBased::Comment(Range {
                    start: "/*",
                    end: "*/",
                    exceptions: Vec::new(),
                    eof_allowed: false,
                }),
                RangeBased::String(Range {
                    start: "\"",
                    end: "\"",
                    exceptions: vec!["\\\"".into()],
                    eof_allowed: false,
                }),
                RangeBased::RawString(Range {
                    start: "r#\"",
                    end: "\"#",
                    exceptions: Vec::new(),
                    eof_allowed: false,
                }),
            ],
        }
    }
}

