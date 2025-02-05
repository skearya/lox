use std::collections::VecDeque;

use crate::{
    expr::{Binary, Expr, Grouping, Unary},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'src> {
    tokens: VecDeque<Token<'src>>,
}

impl<'src> Parser<'src> {
    pub fn new(tokens: VecDeque<Token<'src>>) -> Self {
        Self { tokens }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.front()
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn next_if(&mut self, f: impl Fn(&TokenKind) -> bool) -> Option<Token> {
        match self.peek() {
            Some(token) if f(&token.kind) => self.next(),
            _ => None,
        }
    }

    pub fn expression(&mut self) -> Option<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = self.comparison()?;

        while let Some(token) =
            self.next_if(|kind| matches!(kind, TokenKind::BangEqual | TokenKind::EqualEqual))
        {
            let operator = token.kind.into();
            let right = self.comparison()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Some(expr)
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = self.term()?;

        while let Some(token) = self.next_if(|kind| {
            matches!(
                kind,
                TokenKind::Greater
                    | TokenKind::GreaterEqual
                    | TokenKind::Less
                    | TokenKind::LessEqual
            )
        }) {
            let operator = token.kind.into();
            let right = self.term()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Some(expr)
    }

    fn term(&mut self) -> Option<Expr> {
        let mut expr = self.factor()?;

        while let Some(token) =
            self.next_if(|kind| matches!(kind, TokenKind::Minus | TokenKind::Plus))
        {
            let operator = token.kind.into();
            let right = self.factor()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Some(expr)
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;

        while let Some(token) =
            self.next_if(|kind| matches!(kind, TokenKind::Slash | TokenKind::Star))
        {
            let operator = token.kind.into();
            let right = self.unary()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if let Some(token) = self.next_if(|kind| matches!(kind, TokenKind::Bang | TokenKind::Minus))
        {
            let operator = token.kind.into();
            let right = self.unary()?;

            Some(Expr::Unary(Box::new(Unary::new(operator, right))))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        if let Some(Token {
            kind: TokenKind::Literal(literal),
            ..
        }) = self.next_if(|kind| matches!(kind, TokenKind::Literal(_)))
        {
            return Some(Expr::Literal(literal));
        }

        if self
            .next_if(|kind| matches!(kind, TokenKind::LeftParen))
            .is_some()
        {
            let expr = self.expression()?;

            if !self
                .next()
                .is_some_and(|token| matches!(token.kind, TokenKind::RightParen))
            {
                panic!("expected ')'")
            }

            Some(Expr::Grouping(Box::new(Grouping::new(expr))))
        } else {
            None
        }
    }

    fn synchronize(&mut self) {
        while let Some(Token { kind, .. }) = self.peek() {
            match kind {
                TokenKind::Semicolon => {
                    self.next();
                    return;
                }
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                _ => {
                    self.next();
                }
            }
        }
    }
}
