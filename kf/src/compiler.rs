// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::cbackend;
use crate::comp_msg;
use crate::kf_core;
use crate::lang_def;
use crate::lexer;
use crate::parser;

pub const BUILT_IN_STUFF: &str = r#"
fn print(s: &str, ...) {}
fn println(s: &str, ...) {}
fn assert(cond: bool, s: &str, ...) {}
fn panic(s: &str, ...) {}
fn exit(code: i8) {}
fn sleep(milisecs: i32) {}
fn env(name: &str) -> str {}
fn file_to_str(filename: &str) -> Result<str, Error> {}
fn file_to_bytes(filename: &str) -> Result<Vec<u8>, Error> {}
fn file_exists(filename: &str) -> bool {}

enum Result<R, E> {
    Ok(R),
    Err(E)
}

enum Option<R> {
    Some(R),
    None,
}
"#;

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

fn process_errors(code: &str, errors: &comp_msg::CompileMsgCol) -> String {
    let lines = code.lines().collect::<Vec<&str>>();
    let mut err_log = String::new();
    for e in errors {
        if let Some(at) = e.at {
            if at.line > 0 && at.line <= lines.len() {
                let l = lines[at.line - 1];
                err_log += l;
                err_log += "\n";

                let mut arrow = String::new();
                if at.col > 0 {
                    arrow += &" ".repeat(at.col - 1);
                }
                arrow += "^\n";
                err_log += &arrow;
            }
            err_log += &format!("error: {} <source>:{}:{}\n", e.msg, at.line, at.col);
        } else {
            err_log += &format!("error: {}\n", e.msg);
        }
        if e.details.len() > 0 {
            err_log += &format!("details: {}\n", e.details);
        }
    }
    err_log
}
