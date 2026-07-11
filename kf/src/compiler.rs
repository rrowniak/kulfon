// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 25.06.2024
// ---------------------------------------------------
use crate::cbackend;
use crate::comp_msg;
use crate::kf_core;
use crate::lex_structs;
use crate::lexer;
use crate::parser;

/// This string contains built-in predefined functions and definitions
/// that are part of the Kulfon language.
pub const BUILT_IN_STUFF: &str = include_str!("builtin.kf");

/// Compiles a single Kulfon source code.
///
/// # Arguments
///
/// * `input` - The path to the Kulfon source code.
/// * `output` - The path to the generated output C source file.
///
/// # Returns
/// A `Result<(), String>`, `Ok(())` if everything fine, otherwise formatted error messages
/// `Err(String)`.
pub fn compile_single(input: &std::path::Path, output: &std::path::Path) -> Result<(), String> {
    let kulfon_lang = lex_structs::Lang::new();
    // parse built-in stuff
    let (tokens, errors) = lexer::tokenize(&kulfon_lang, BUILT_IN_STUFF);
    if errors.len() > 0 {
        panic!("{:?}", errors);
    }
    let built_in_ast = parser::parse(&tokens).expect("Built-in stuff parsing failure");
    // parse user source code
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

    let int_repr = match kf_core::InterRepr::from(ast, built_in_ast) {
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

/// Formats a list of compilation errors into a human-readable message suitable for terminal display.
///
/// This function takes the original source code and a collection of compiler messages,
/// and produces a formatted string highlighting each error. The output typically includes:
///
/// - Line and column information
/// - Code snippets with carets (`^`) marking the error locations
/// - Error messages describing the issue
///
/// The formatted result is designed to help developers quickly locate and understand
/// problems in their Kulfon source code.
///
/// # Arguments
///
/// * `code` - The original Kulfon source code as a string slice. Used to extract and display relevant lines.
/// * `errors` - A collection of compilation messages returned during the parsing or analysis phase.
///
/// # Returns
///
/// A `String` containing a formatted diagnostic report that can be printed to a terminal or log.
///
/// # Example
///
/// ```rust
/// let code = r#"let x = 5
/// let y = x + "#;
///
/// let errors = parser::parse(code).unwrap_err();
/// println!("{}", process_errors(code, &errors));
/// ```
///
/// # See Also
///
/// - [`comp_msg::CompileMsgCol`] for the structure of the error collection.
/// - [`TextPoint`] for line and column positioning metadata used in the formatting.
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
