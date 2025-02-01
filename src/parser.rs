use crate::token::{Token, TokenKind};

#[derive(Debug)]
struct Parser<'src, 'tokens> {
    tokens: &'tokens [Token<'src>],
    current: usize,
}

impl<'src, 'tokens> Parser<'src, 'tokens> {
    fn new(tokens: &'tokens [Token<'src>]) -> Self {
        Self { tokens, current: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn peek_peek(&self) -> Option<&Token> {
        self.tokens.get(self.current + 1)
    }

    fn next(&mut self) -> Option<&Token> {
        self.tokens.get(self.current).inspect(|_| self.current += 1)
    }

    fn expression(&mut self) {
        self.equality();
    }

    fn equality(&mut self) {
        // let expr = comparison();

        match self.next() {
            Some(
                token @ Token {
                    kind: TokenKind::BangEqual | TokenKind::EqualEqual,
                    ..
                },
            ) => {
                self.next();
            }
            _ => {}
        }
    }
}
