// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.06.2024
// ---------------------------------------------------

mod lexer;
mod lang_def;
mod bnf_parser;
mod parse_iter;
mod parser;
mod ast;

fn main() {
    let kulfon_lang = lang_def::Lang::new();
    lexer::tokenize(&kulfon_lang, "");
    _ = bnf_parser::parse("");
    _ = parser::parse("");
}
