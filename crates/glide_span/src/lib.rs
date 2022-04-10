pub mod source;

use std::fmt;

use crate::source::Source;

#[derive(Copy, Clone)]
pub struct Span<'a> {
    source: &'a Source,
    start: usize,
    end: usize,
}

impl<'a> fmt::Debug for Span<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"@{}:{}", self.data(), self.start, self.end)
    }
}

impl<'a> Span<'a> {
    pub fn full(source: &'a Source) -> Self {
        Self {
            source,
            start: 0,
            end: source.len(),
        }
    }

    pub fn data(self) -> &'a str {
        &self.source.data[self.start..self.end]
    }

    pub fn advance(&mut self, count: usize) -> Self {
        assert!(self.start + count <= self.end);
        let advanced = Self {
            source: self.source,
            start: self.start,
            end: self.start + count,
        };
        self.start += count;
        advanced
    }

    pub fn len(self) -> usize {
        self.end - self.start
    }

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }
}
