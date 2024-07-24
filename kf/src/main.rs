// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.06.2024
// ---------------------------------------------------

mod ast;
mod cbackend;
mod compiler;
mod comp_msg;
mod lang_def;
mod lexer;
mod kf_core;
mod parse_iter;
mod parser;
mod type_system;

const VERSION: &str = "0.0.1";

macro_rules! fail {
    () => {
        std::process::exit(1);
    };
    ($($arg:tt)*) => {
        eprintln!("{}", format!($($arg)*));
        std::process::exit(1);
    };
}

fn main() {
    parse_1st();
}

fn parse_1st() {
    let mut arg_iter = std::env::args();
    arg_iter.next();
    if let Some(arg_1) = arg_iter.next() {
        match arg_1.as_str() {
            "-h" | "--help" => help_message(),
            "-V" | "--version" => print_version(),
            "compile" => compile(&mut arg_iter),
            "build" => print_not_supported_yet(),
            "run" => print_not_supported_yet(),
            "clean" => print_not_supported_yet(),
            "check" => print_not_supported_yet(),
            "test" => print_not_supported_yet(),
            "doc" => print_not_supported_yet(),
            _ => help_message(),
        }
    } else {
        help_message();
        std::process::exit(1);
    }
}

fn compile(iter: &mut std::env::Args) {
    let mut source_in = None;
    let mut source_out = None;

    while let Some(arg) = iter.next() {
        match arg.as_str() {
            "-i" | "--input" => source_in = iter.next(),
            "-o" | "--output" => source_out = iter.next(),
            o => {
                fail!("Unknown option '{}'", o);
            }
        }
    }

    let source_in = source_in.unwrap_or_else(|| {
        fail!("Input option (Kulfon source file) missing. Consider passing '--input <FILE>'");
    });

    let source_out = source_out.unwrap_or_else(|| {
        fail!("Output option (C source file) missing. Consider passing '--output <FILE>'");
    });

    if let Err(e) = compiler::compile_single(
        std::path::Path::new(&source_in),
        std::path::Path::new(&source_out),
    ) {
        fail!("{}", e);
    }
}

fn help_message() {
    let msg = r#"
Usage: kf [OPTIONS] <SUBCOMMAND>

OPTIONS:
    -h, --help                  Print this help message
    -V, --version               Print the version of the Kulfon compiler

SUBCOMMANDS:
    compile                     Compile a single Kulfon source file
    build                       Compile the Kulfon project
    run                         Compile and run the Kulfon project
    clean                       Clean the build artifacts
    check                       Check the Kulfon project for errors without compiling
    test                        Run tests for the Kulfon project
    doc                         Generate documentation for the Kulfon project

SUBCOMMAND OPTIONS:

    compile
        Usage: kf compile [OPTIONS]
        Options:
            -i, --input <FILE>  Input Kulfon source file
            -o, --output <FILE> C source file generated by the Kulfon compiler

    build
        Usage: kf build [OPTIONS]
        Options:
            --release           Build artifacts in release mode, with optimizations
            -o, --output <DIR>  Specify the output directory for build artifacts

    run
        Usage: kf run [OPTIONS]
        Options:
            --release           Run the project in release mode, with optimizations
            -a, --args <ARGS>   Pass additional arguments to the executable

    clean
        Usage: kf clean [OPTIONS]
        Options:
            -o, --output <DIR>  Specify the output directory to clean

    check
        Usage: kf check [OPTIONS]
        Options:
            --warnings-as-errors  Treat warnings as errors

    test
        Usage: kf test [OPTIONS]
        Options:
            --release           Run tests in release mode, with optimizations
            -a, --args <ARGS>   Pass additional arguments to the test runner

    doc
        Usage: kf doc [OPTIONS]
        Options:
            -o, --output <DIR>  Specify the output directory for documentation

EXAMPLES:
    Compile a single Kulfon source file:
        kf compile -i hello.kf -o hello.c

    Compile the Kulfon project:
        kf build

    Compile and run the Kulfon project:
        kf run

    Clean the build artifacts:
        kf clean

    Check the Kulfon project for errors:
        kf check

    Run tests for the Kulfon project:
        kf test

    Generate documentation for the Kulfon project:
        kf doc

AUTHOR:
    Kulfon Compiler is developed by Rafał Równiak.
"#;
    println!("Kulfon compiler v{}\n{}", VERSION, msg);
}

fn print_version() {
    println!("Kulfon compiler v{} by Rafał Równiak (c) 2024\n", VERSION);
}

fn print_not_supported_yet() {
    println!("Sorry, this feature is not supported yet\n");
}
