// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.06.2024
// ---------------------------------------------------
use crate::comp_msg::TextPoint;
use crate::lex_def::{KfTokKind, SPEC_SYMBOLS};

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
    pub special_sym: Vec<&'a str>,
    pub range_based: Vec<RangeBased<'a>>,
}

impl<'a> Lang<'a> {
    pub fn new() -> Lang<'a> {
        let mut sym = SPEC_SYMBOLS.to_vec();
        // sort by length starting from the longer symobol
        // that order is needed by lexer
        sym.sort_by(|a, b| b.len().cmp(&a.len()));

        Lang {
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

    pub fn is_keyword(&self, s: &str) -> bool {
        match KfTokKind::from(s) {
            Some(t) => t.is_keyword() || t.is_special_literal(),
            None => false,
        }
    }
}
