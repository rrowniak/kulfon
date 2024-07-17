// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::cbackend;
use crate::lang_def;
use crate::lexer;
use crate::parser;
use crate::kf_core;

pub fn compile_single(input: &std::path::Path, output: &std::path::Path) -> Result<(), String> {
    let kulfon_lang = lang_def::Lang::new();
    let input_source = std::fs::read_to_string(&input);
    let input_source = match input_source {
        Ok(content) => content,
        Err(e) => {
            return Err(format!(
                "Reading input '{}' error: {}",
                input.to_str().expect("Input file path corrupted"),
                e
            ))
        }
    };
    let (tokens, errors) = lexer::tokenize(&kulfon_lang, &input_source);
    if errors.len() > 0 {
        return Err(process_errors(&input_source, &errors));
    }
    let ast = match parser::parse(&tokens) {
        Ok(ast) => ast,
        Err(e) => {
            return Err(process_errors(&input_source, &e));
        }
    };

    let int_repr = match kf_core::InterRepr::from(ast) {
        Ok(int) => int,
        Err(e) => return Err(process_errors(&input_source, &e)),
    };

    let c_code = cbackend::gen_c_code(&int_repr)?;

    if let Err(e) = std::fs::write(&output, c_code) {
        return Err(format!(
            "Writing output (C file) '{}' error: {}",
            output.to_str().expect("Output file path corrupted"),
            e
        ));
    }
    Ok(())
}

fn process_errors(code: &str, errors: &Vec<lang_def::ParsingError>) -> String {
    let lines = code.lines().collect::<Vec<&str>>();
    let mut err_log = String::new();
    for e in errors {
        if e.at.line > 0 && e.at.line <= lines.len() {
            let l = lines[e.at.line - 1];
            err_log += l;
            err_log += "\n";

            let mut arrow = String::new();
            if e.at.col > 0 {
                arrow += &" ".repeat(e.at.col - 1);
            }
            arrow += "^\n";
            err_log += &arrow;
        }
        let msg = format!("error: {} <source>:{}:{}\n", e.msg, e.at.line, e.at.col);
        err_log += &msg;
        if e.details.len() > 0 {
            err_log += &format!("details: {}\n", e.details);
        }
    }
    err_log
}
