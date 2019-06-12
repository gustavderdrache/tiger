// use enum_kinds::Enum

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn extend(self, other: Span) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn kind<'a, K>(&'a self) -> K
    where
        &'a T: Into<K>,
    {
        (&self.data).into()
    }
}
