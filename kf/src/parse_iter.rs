// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 21.06.2024
// ---------------------------------------------------
pub struct ParseIter<'a, T> {
    vec: &'a [T],
    pos_of_next: usize,
}

impl<'a, T> ParseIter<'a, T> {
    pub fn new(vec: &'a [T]) -> Self {
        Self {
            vec,
            pos_of_next: 0,
        }
    }

    pub fn peek(&self) -> Option<&'a T> {
        self.lookahead(0)
    }

    pub fn lookahead(&self, offset: usize) -> Option<&'a T> {
        if self.pos_of_next == 0 {
            // iterator is not started yet
            return None;
        }
        if self.pos_of_next + offset - 1 < self.vec.len() {
            return Some(&self.vec[self.pos_of_next + offset - 1]);
        }
        None
    }
}

impl<'a, T> Iterator for ParseIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        self.pos_of_next += 1;
        if self.pos_of_next > self.vec.len() {
            return None;
        }
        let item = &self.vec[self.pos_of_next - 1];
        Some(item)
    }
}

