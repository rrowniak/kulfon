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
#[rustfmt::skip]
const KULFON_KEYWORDS: &[&str] = &[
    "if", "else", "loop", "for", "while",
    "fn"
];
const KULFON_RES_KEYWORDS: &[&str] = RUST_KEYWORDS;
#[rustfmt::skip]
const KULFON_SPEC_SYMBOLS: &[&str] = &[
    // Single-character symbols
    "{", "}", "(", ")", "[", "]", "<", ">", "+", "-", "*", "/", "%", "^", "!", "&", "|", "~", "=",
    "@", "#", "$", "?", ";", ":", ",", ".", "'", "\"", "_",
    // Multi-character operators and symbols
    "->", "=>", "::", "..", "...", "..=", "==", "!=", ">=", "<=", "&&", "||", "<<", ">>", "+=", 
    "-=", "*=", "/=", "%=", "^=", "&=", "|=", "<<=", ">>=", "++", "--", 
    // Attribute and macro-related
    "#!", "#[", "]", "!", "?"
];

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
    DocComment(Range),
    Comment(Range),
    String(Range),
    RawString(Range),
}

impl RangeBased {
    pub fn get_range(&self) -> &Range {
        match self {
            RangeBased::LineComment(r) => r,
            RangeBased::DocComment(r) => r,
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
    pub fn new_empty() -> Lang {
        Lang {
            keywords: Vec::new(),
            reserved_keywords: Vec::new(),
            special_sym: Vec::new(),
            range_based: Vec::new(),
        }
    }
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

#[derive(Copy, Clone)]
pub struct TextPoint {
    pub line: usize,
    pub col: usize,
}

pub struct ParsingError {
    pub msg: String,
    pub details: String,
    pub at: TextPoint,
}
