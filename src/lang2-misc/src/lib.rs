use std::ops::Range;

use serde::{Deserialize, Serialize};

pub type Pos = u32;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

#[inline(always)]
pub fn try_from_to<T>(start: T, end: T, offset: Pos) -> Option<Span>
where
    Pos: TryFrom<T>,
{
    Some(Span {
        start: Pos::try_from(start).ok()? + offset,
        end: Pos::try_from(end).ok()? + offset,
    })
}

impl Span {
    #[inline(always)]
    pub fn as_usize_range(self, offset: Pos) -> Range<usize> {
        (self.start - offset) as usize..(self.end - offset) as usize
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}

#[derive(Debug, Serialize)]
pub enum Error {
    ExpectedActual {
        expected: &'static str,
        actual: &'static str,
        pos: Pos,
    },
    UnexpectedChar {
        pos: Pos,
    },
    UnexpectedEof {
        pos: Pos,
    },
}

#[derive(Debug, Serialize)]
pub struct Errors {
    errors: Vec<Error>,
}

impl Errors {
    pub fn new() -> Errors {
        Self { errors: vec![] }
    }

    pub fn add(&mut self, error: Error) {
        self.errors.push(error);
    }
}
