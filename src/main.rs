use std::{collections::VecDeque, iter::Peekable, mem, slice::Iter};

#[derive(Clone, PartialEq, Eq)]
enum TokenKind {
    Var,
    String(String),
}

struct Parser<I: Iterator<Item = TokenKind>> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = TokenKind>> Parser<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn take(&mut self, tokens: &[TokenKind]) -> Option<TokenKind> {
        let current = self.tokens.peek()?;

        if tokens.iter().any(|token| token == current) {
            self.tokens.next()
        } else {
            None
        }
    }
}

fn main() {}
