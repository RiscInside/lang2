use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[inline(always)]
pub fn from_to<T>(start: T, end: T) -> Span
where
    u32: TryFrom<T>,
{
    Span {
        start: start
            .try_into()
            .unwrap_or_else(|_| panic!("source pos out of range")),
        end: end
            .try_into()
            .unwrap_or_else(|_| panic!("source pos out of range")),
    }
}
pub trait HasSpan {
    fn span(&self) -> Span;
}

impl From<std::ops::Range<usize>> for Span {
    #[inline(always)]
    fn from(value: std::ops::Range<usize>) -> Self {
        from_to(value.start, value.end)
    }
}

impl Span {
    #[inline(always)]
    pub fn as_usize_range(self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}
