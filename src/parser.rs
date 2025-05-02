use std::iter::Peekable;

use crate::{
    ast::{Assign, Binary, Expr, Grouping, If, Logical, Stmt, Unary, Var, While},
    error,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    src: &'src str,
    tokens: Peekable<I>,
}

// Our parser has two kinds of errors
// 1. Errors that don't break parsing, ex: parsing more than 255 function arguments
// 2. Errors that do break parsing, ex: random tokens after `for`
// In case 1, we carry on and log the error
// In case 2, we have to early return and synchronize the parser to a better starting point

type Result<T> = core::result::Result<T, Synchronize>;

#[derive(Debug)]
pub struct Synchronize;

impl<'src, I: Iterator<Item = Token<'src>>> Parser<'src, I> {
    pub fn new(src: &'src str, tokens: I) -> Self {
        Self {
            src,
            tokens: tokens.peekable(),
        }
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.tokens.peek().map(|token| &token.kind)
    }

    fn next(&mut self) -> Option<Token<'src>> {
        self.tokens.next()
    }

    fn take<const N: usize>(&mut self, kinds: [TokenKind; N]) -> Option<Token<'src>> {
        let current = self.peek()?;

        if kinds.iter().any(|token| token == current) {
            if let Some(token) = self.next() {
                Some(token)
            } else {
                unreachable!()
            }
        } else {
            None
        }
    }

    fn eat<const N: usize>(&mut self, kinds: [TokenKind; N]) -> bool {
        self.take(kinds).is_some()
    }

    fn consume(&mut self, expected: TokenKind, error: &'static str) -> Result<Token<'src>> {
        match self.tokens.peek() {
            // Non-owning version of self.take() so the error handler can use `expected`
            Some(Token { kind, .. }) if *kind == expected => {
                if let Some(token) = self.next() {
                    Ok(token)
                } else {
                    unreachable!()
                }
            }
            Some(unexpected) => {
                error::unexpected(&expected, unexpected, self.src, error);

                Err(Synchronize)
            }
            None => Err(Synchronize),
        }
    }

    fn declaration(&mut self) -> Result<Stmt> {
        let stmt = if self.eat([TokenKind::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if stmt.is_err() {
            self.synchronize();
        }

        stmt
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let token = self.consume(TokenKind::Identifier, "Expected variable name")?;

        let name = token.lexeme.to_owned();

        let initalizer = if self.eat([TokenKind::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenKind::Semicolon,
            "Expected semicolon after var declaration",
        )?;

        Ok(Stmt::Var(Var::new(name, initalizer)))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match () {
            () if self.eat([TokenKind::Print]) => self.print_statement(),
            () if self.eat([TokenKind::LeftBrace]) => self.block(),
            () if self.eat([TokenKind::If]) => self.if_statement(),
            () if self.eat([TokenKind::While]) => self.while_statement(),
            () if self.eat([TokenKind::For]) => self.for_statement(),
            () => self.expression_statement(),
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;

        self.consume(TokenKind::Semicolon, "Expected semicolon after value")?;

        Ok(Stmt::Print(expr))
    }

    fn block(&mut self) -> Result<Stmt> {
        Ok(Stmt::Block(self.statements()?))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LeftParen, "Expected left parenthesis after if")?;

        let condition = self.expression()?;

        self.consume(
            TokenKind::RightParen,
            "Expected right parenthesis after if condition",
        )?;

        let then_stmt = self.statement()?;

        let else_stmt = if self.eat([TokenKind::Else]) {
            Some(self.statement()?)
        } else {
            None
        };

        Ok(Stmt::If(Box::new(If::new(condition, then_stmt, else_stmt))))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(
            TokenKind::LeftParen,
            "Expected left parenthesis after while",
        )?;

        let condition = self.expression()?;

        self.consume(
            TokenKind::RightParen,
            "Expected right parenthesis after while",
        )?;

        let body = self.statement()?;

        Ok(Stmt::While(Box::new(While::new(condition, body))))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LeftParen, "Expected parenthesis after for")?;

        let initializer = match () {
            () if self.eat([TokenKind::Semicolon]) => None,
            () if self.eat([TokenKind::Var]) => Some(self.var_declaration()?),
            () => Some(self.expression_statement()?),
        };

        let condition = if self.eat([TokenKind::Semicolon]) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(
            TokenKind::Semicolon,
            "Expected semicolon after loop condition",
        )?;

        let increment = if self.eat([TokenKind::Semicolon]) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(TokenKind::RightParen, "Expected left parenthesis after for")?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expr(increment)]);
        }

        if let Some(condition) = condition {
            body = Stmt::While(Box::new(While::new(condition, body)));
        }

        if let Some(initalizer) = initializer {
            body = Stmt::Block(vec![initalizer, body]);
        }

        Ok(body)
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;

        self.consume(TokenKind::Semicolon, "Expected semicolon after expression")?;

        Ok(Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if self.eat([TokenKind::Equal]) {
            let value = self.assignment()?;

            if let Expr::Variable(name) = expr {
                Ok(Expr::Assign(Box::new(Assign::new(name, value))))
            } else {
                // TODO: Invalid assignment target error
                Ok(expr)
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;

        while let Some(token) = self.take([TokenKind::Or]) {
            let kind = token.kind.into();
            let right = self.and()?;

            expr = Expr::Logical(Box::new(Logical::new(expr, kind, right)));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while let Some(token) = self.take([TokenKind::And]) {
            let kind = token.kind.into();
            let right = self.equality()?;

            expr = Expr::Logical(Box::new(Logical::new(expr, kind, right)));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while let Some(token) = self.take([TokenKind::BangEqual, TokenKind::EqualEqual]) {
            let operator = token.kind.into();
            let right = self.comparison()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
        let mut expr = self.term()?;

        while let Some(token) = self.take([
            TokenKind::Greater,
            TokenKind::GreaterEqual,
            TokenKind::Less,
            TokenKind::LessEqual,
        ]) {
            let operator = token.kind.into();
            let right = self.term()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while let Some(token) = self.take([TokenKind::Minus, TokenKind::Plus]) {
            let operator = token.kind.into();
            let right = self.factor()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(token) = self.take([TokenKind::Slash, TokenKind::Star]) {
            let operator = token.kind.into();
            let right = self.unary()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(token) = self.take([TokenKind::Bang, TokenKind::Minus]) {
            let operator = token.kind.into();
            let right = self.unary()?;

            Ok(Expr::Unary(Box::new(Unary::new(operator, right))))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr> {
        // Check if the next token is a literal and then call .next() to take ownership of the literal
        if let Some(TokenKind::Literal(_literal)) = self.peek() {
            let Some(Token {
                kind: TokenKind::Literal(literal),
                ..
            }) = self.next()
            else {
                unreachable!()
            };

            return Ok(Expr::Literal(literal));
        }

        if let Some(token) = self.take([TokenKind::Identifier]) {
            return Ok(Expr::Variable(token.lexeme.to_owned()));
        }

        self.consume(TokenKind::LeftParen, "Expected expression")?;

        let expr = self.expression()?;

        self.consume(
            TokenKind::RightParen,
            "Expected parenthesis after expression",
        )?;

        Ok(Expr::Grouping(Box::new(Grouping::new(expr))))
    }

    fn synchronize(&mut self) {
        while let Some(kind) = self.peek() {
            match kind {
                TokenKind::Class
                | TokenKind::Fun
                | TokenKind::Var
                | TokenKind::For
                | TokenKind::If
                | TokenKind::While
                | TokenKind::Print
                | TokenKind::Return => return,
                TokenKind::Semicolon => {
                    self.next();
                    return;
                }
                _ => {
                    self.next();
                }
            }
        }
    }

    pub fn statements(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = vec![];

        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            statements.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "Expected right brace")?;

        Ok(statements)
    }
}

impl<'src, I: Iterator<Item = Token<'src>>> Iterator for Parser<'src, I> {
    type Item = Stmt;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.peek().is_some() {
                if let Ok(stmt) = self.declaration() {
                    break Some(stmt);
                }
            } else {
                break None;
            }
        }
    }
}
