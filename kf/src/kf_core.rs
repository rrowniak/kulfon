// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 04.07.2024
// ---------------------------------------------------
use crate::ast;
use crate::lang_def::ParsingError;

pub struct InterRepr {
    pub syntree: ast::Node,
}

impl InterRepr {
    pub fn from(syntree: ast::Node) -> Self {
        Self{ syntree }
    }

    pub fn compile(&mut self) -> Result<(), Vec<ParsingError>> {
        Ok(())
    } 

    fn first_pass(&mut self) {
        // evaluate expression types
    }

    fn second_pass(&mut self) { }
}
