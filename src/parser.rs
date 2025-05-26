use std::iter::Peekable;

use crate::{
    ast::{
        Assign, Binary, Call, Class, Expr, Function, Get, Grouping, If, Logical, Set, Stmt, Unary,
        Var, While,
    },
    error,
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'src, I: Iterator<Item = Token<'src>>> {
    pub errored: bool,
    pub src: &'src str,
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
            errored: false,
            src,
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(mut self) -> Option<Vec<Stmt>> {
        let mut statements = vec![];

        while self.peek().is_some() {
            if let Ok(stmt) = self.declaration() {
                statements.push(stmt);
            }
        }

        if self.errored { None } else { Some(statements) }
    }

    fn peek(&mut self) -> Option<&TokenKind> {
        self.tokens.peek().map(|Token { kind, .. }| kind)
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

    fn consume(&mut self, expected: TokenKind, error: &str) -> Result<Token<'src>> {
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
                self.errored = true;
                error::unexpected(&expected, unexpected, self.src, error);

                Err(Synchronize)
            }
            None => {
                self.errored = true;
                Err(Synchronize)
            }
        }
    }

    fn declaration(&mut self) -> Result<Stmt> {
        let stmt = match () {
            () if self.eat([TokenKind::Class]) => self.class_declaration(),
            () if self.eat([TokenKind::Fun]) => self.function("function").map(Stmt::Function),
            () if self.eat([TokenKind::Var]) => self.var_declaration(),
            () => self.statement(),
        };

        if stmt.is_err() {
            self.synchronize();
        }

        stmt
    }

    fn class_declaration(&mut self) -> Result<Stmt> {
        let name = self
            .consume(TokenKind::Identifier, "Expected class name")?
            .lexeme
            .to_owned();

        self.consume(TokenKind::LeftBrace, "Expected '{' before class body")?;

        let mut methods = vec![];

        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            methods.push(self.function("method")?);
        }

        self.consume(TokenKind::RightBrace, "Expected '}' after class body")?;

        Ok(Stmt::Class(Class::new(name, methods)))
    }

    fn function(&mut self, kind: &str) -> Result<Function> {
        let name = self
            .consume(TokenKind::Identifier, &format!("Expected {kind} name"))?
            .lexeme
            .to_owned();

        self.consume(
            TokenKind::LeftParen,
            &format!("Expected '(' after {kind} name"),
        )?;

        let mut arguments = vec![];

        if !matches!(self.peek(), Some(TokenKind::RightParen)) {
            arguments.push(
                self.consume(TokenKind::Identifier, "Expected parameter name")?
                    .lexeme
                    .to_owned(),
            );

            while self.eat([TokenKind::Comma]) {
                if arguments.len() >= 255 {
                    // TODO: Better error
                    eprintln!("Can't have more than 255 parameters");
                }
                arguments.push(
                    self.consume(TokenKind::Identifier, "Expected parameter name")?
                        .lexeme
                        .to_owned(),
                );
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after parameters")?;

        self.consume(
            TokenKind::LeftBrace,
            &format!("Expect '{{' before {kind} body"),
        )?;

        let body = self.statements()?;

        Ok(Function::new(name, arguments, body))
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self
            .consume(TokenKind::Identifier, "Expected variable name")?
            .lexeme
            .to_owned();

        let initializer = if self.eat([TokenKind::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenKind::Semicolon,
            "Expected semicolon after var declaration",
        )?;

        Ok(Stmt::Var(Var::new(name, initializer)))
    }

    fn statement(&mut self) -> Result<Stmt> {
        match () {
            () if self.eat([TokenKind::Print]) => self.print_statement(),
            () if self.eat([TokenKind::Return]) => self.return_statement(),
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

    fn return_statement(&mut self) -> Result<Stmt> {
        let expr = if matches!(self.peek(), Some(TokenKind::Semicolon)) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(
            TokenKind::Semicolon,
            "Expected semicolon after return value",
        )?;

        Ok(Stmt::Return(expr))
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

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
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

            match expr {
                Expr::Variable(name) => Ok(Expr::Assign(Box::new(Assign::new(name, value)))),
                Expr::Get(get) => Ok(Expr::Set(Box::new(Set::new(get.object, get.name, value)))),
                _ => {
                    // TODO: Better invalid assignment target error
                    self.errored = true;
                    eprintln!("Invalid assignment target: {expr:#?}");

                    Ok(expr)
                }
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
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.eat([TokenKind::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.eat([TokenKind::Dot]) {
                let name = self
                    .consume(TokenKind::Identifier, "Expected property name after '.'")?
                    .lexeme
                    .to_owned();

                expr = Expr::Get(Box::new(Get::new(expr, name)));
            } else {
                break;
            }
        }

        Ok(expr)
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

        if self.eat([TokenKind::This]) {
            return Ok(Expr::This);
        }

        if let Some(token) = self.take([TokenKind::Identifier]) {
            return Ok(Expr::Variable(token.lexeme.to_owned()));
        }

        if self.eat([TokenKind::LeftParen]) {
            let expr = self.expression()?;

            self.consume(
                TokenKind::RightParen,
                "Expected parenthesis after expression",
            )?;

            return Ok(Expr::Grouping(Box::new(Grouping::new(expr))));
        }

        // TODO: Better expected expression error
        self.errored = true;
        eprintln!("Expected expression");

        Err(Synchronize)
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

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = vec![];

        if !matches!(self.peek(), Some(TokenKind::RightParen)) {
            arguments.push(self.expression()?);

            while self.eat([TokenKind::Comma]) {
                if arguments.len() >= 255 {
                    // TODO: Better error
                    eprintln!("Can't have more than 255 arguments");
                }

                arguments.push(self.expression()?);
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after arguments")?;

        Ok(Expr::Call(Box::new(Call::new(callee, arguments))))
    }

    fn statements(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = vec![];

        while !matches!(self.peek(), Some(TokenKind::RightBrace)) {
            statements.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, "Expected right brace")?;

        Ok(statements)
    }
}
