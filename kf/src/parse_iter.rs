// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 21.06.2024
// ---------------------------------------------------

/// An extended iterator for parsing and lexing tasks, supporting lookahead functionality.
///
/// `ParseIter` wraps a slice and implements the standard `Iterator` trait, while also providing
/// the ability to peek at the current and upcoming elements without advancing the iterator. This is especially
/// useful in parsing and lexing scenarios where decisions may depend on future tokens.
///
/// Unlike a typical iterator, `ParseIter` allows inspection of the current and next elements via:
/// - [`peek`] — look at the current element without consuming it
/// - [`lookahead`] — look ahead by an arbitrary number of steps
///
/// # Example
///
/// ```
/// let input = vec!['a', 'b', 'c'];
/// let mut iter = ParseIter::new(&input);
///
/// assert_eq!(iter.next(), Some(&'a'));
/// assert_eq!(iter.peek(), Some(&'a'));
/// assert_eq!(iter.lookahead(1), Some(&'b'));
/// ```
pub struct ParseIter<'a, T> {
    /// Slice of the input collection being iterated over.
    vec: &'a [T],
    /// Index of the next element to be returned.
    pos_of_next: usize,
}

impl<'a, T> ParseIter<'a, T> {
    /// Constructs a new `ParseIter` from a slice.
    ///
    /// # Arguments
    ///
    /// * `vec` – A shared slice of items to iterate over.
    ///
    /// # Returns
    ///
    /// A `ParseIter` instance initialized to the start of the slice.
    pub fn new(vec: &'a [T]) -> Self {
        Self {
            vec,
            pos_of_next: 0,
        }
    }
    
    /// Returns a reference to the current item in the iterator without advancing it.
    ///
    /// This is equivalent to calling [`lookahead(0)`], and allows access to the current element
    /// without consuming it. This can be especially useful when passing the iterator into
    /// helper functions that need to inspect the current element but don't want to advance it.
    ///
    /// Note that you can call `peek` multiple times.
    ///
    /// # Returns
    ///
    /// - `Some(&T)` if the iterator has been started via [`next()`] and has not been exhausted.
    /// - `None` if the iterator has not yet advanced or has already reached the end.
    ///
    /// # Example
    ///
    /// ```
    /// let data = vec!['a', 'b'];
    /// let mut iter = ParseIter::new(&data);
    ///
    /// assert_eq!(iter.peek(), None); // not started yet
    /// iter.next();
    /// assert_eq!(iter.peek(), Some(&'a')); // current item
    /// assert_eq!(iter.peek(), Some(&'a')); // next call also yield with current item
    /// ```
    pub fn peek(&self) -> Option<&'a T> {
        self.lookahead(0)
    }
    
    /// Returns a reference to the item at the given offset ahead of the current position,
    /// without advancing the iterator.
    ///
    /// This method allows inspecting elements beyond the current one, which is particularly
    /// useful in parsing and lexing scenarios where upcoming tokens affect control flow.
    ///
    /// The offset is relative to the current position **after** the most recent call to [`next()`].
    /// For example, `lookahead(0)` returns the same item as [`peek()`], i.e., the most recently
    /// returned element. `lookahead(1)` returns the next element that would be returned by `next()`,
    /// and so on.
    ///
    /// # Arguments
    ///
    /// * `offset` – Number of elements ahead of the current one to inspect (0 = current).
    ///
    /// # Returns
    ///
    /// - `Some(&T)` if the offset is within bounds and the iterator has started.
    /// - `None` if the iterator has not yet started or the requested offset is out of bounds.
    ///
    /// # Example
    ///
    /// ```
    /// let data = vec![10, 20, 30, 40];
    /// let mut iter = ParseIter::new(&data);
    ///
    /// assert_eq!(iter.lookahead(0), None); // not started yet
    /// iter.next(); // now pointing at 10
    ///
    /// assert_eq!(iter.lookahead(0), Some(&10));
    /// assert_eq!(iter.lookahead(1), Some(&20));
    /// assert_eq!(iter.lookahead(2), Some(&30));
    /// assert_eq!(iter.lookahead(3), Some(&40));
    /// assert_eq!(iter.lookahead(4), None); // out of bounds
    /// ```
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

    /// Advances the iterator and returns the next item, if any.
    ///
    /// This method moves the internal cursor forward and returns the corresponding
    /// item from the underlying slice.
    fn next(&mut self) -> Option<Self::Item> {
        self.pos_of_next += 1;
        if self.pos_of_next > self.vec.len() {
            return None;
        }
        let item = &self.vec[self.pos_of_next - 1];
        Some(item)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_iteration() {
        let data = vec![1, 2, 3];
        let mut iter = ParseIter::new(&data);

        assert_eq!(iter.next(), Some(&1));
        assert_eq!(iter.next(), Some(&2));
        assert_eq!(iter.next(), Some(&3));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_peek_and_lookahead() {
        let data = vec!['a', 'b', 'c', 'd'];
        let mut iter = ParseIter::new(&data);

        assert_eq!(iter.peek(), None); // Not started yet
        assert_eq!(iter.lookahead(0), None);
        assert_eq!(iter.lookahead(1), None);

        // Start iteration
        assert_eq!(iter.next(), Some(&'a'));
        assert_eq!(iter.peek(), Some(&'a'));
        assert_eq!(iter.lookahead(0), Some(&'a'));
        assert_eq!(iter.lookahead(1), Some(&'b'));
        assert_eq!(iter.lookahead(2), Some(&'c'));
        assert_eq!(iter.lookahead(3), Some(&'d'));
        assert_eq!(iter.lookahead(4), None); // out of bounds

        iter.next(); // consume 'b'
        assert_eq!(iter.peek(), Some(&'b'));
    }

    #[test]
    fn test_empty_slice() {
        let data: Vec<i32> = vec![];
        let mut iter = ParseIter::new(&data);

        assert_eq!(iter.next(), None);
        assert_eq!(iter.peek(), None);
        assert_eq!(iter.lookahead(0), None);
        assert_eq!(iter.lookahead(1), None);
    }

    #[test]
    fn test_peek_after_end() {
        let data = vec![42];
        let mut iter = ParseIter::new(&data);

        assert_eq!(iter.next(), Some(&42));
        assert_eq!(iter.next(), None);
        assert_eq!(iter.peek(), None);
        assert_eq!(iter.lookahead(0), None);
        assert_eq!(iter.lookahead(1), None);
    }

    #[test]
    fn test_multiple_peek_without_next() {
        let data = vec!['x', 'y', 'z'];
        let mut iter = ParseIter::new(&data);

        // peek before starting - should be None
        assert_eq!(iter.peek(), None);
        assert_eq!(iter.lookahead(0), None);

        iter.next(); // consume 'x'

        // peek and lookahead now work
        assert_eq!(iter.peek(), Some(&'x'));
        assert_eq!(iter.lookahead(1), Some(&'y'));
    }
}
