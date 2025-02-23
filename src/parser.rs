use std::collections::VecDeque;

use crate::{
    ast::{Assign, Binary, Expr, Grouping, If, Logical, Stmt, Unary, Var, While},
    token::{Token, TokenKind},
};

#[derive(Debug)]
pub struct Parser<'src> {
    tokens: VecDeque<Token<'src>>,
}

#[derive(Debug)]
pub enum ParseError {
    ExpectVariableName,
    ExpectSemiAfterVarDecl,
    ExpectSemiAfterValue,
    ExpectSemiAfterExpression,
    ExpectParenAfterExpression,
    ExpectExpression,
    ExpectRightBrace,
    InvalidAssignmentTarget,
    ExpectLeftParenAfterIf,
    ExpectRightParenAfterIfCond,
    ExpectLeftParenAfterWhile,
    ExpectRightParenAfterWhile,
    ExpectParenAfterFor,
    ExpectSemicolonAfterLoopCond,
    ExpectLeftParenAfterFor,
}

type Result<T> = core::result::Result<T, ParseError>;

impl<'src> Parser<'src> {
    pub fn new(tokens: VecDeque<Token<'src>>) -> Self {
        Self { tokens }
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.front().map(|token| &token.kind)
    }

    fn next(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }

    fn next_if(&mut self, f: impl Fn(&TokenKind) -> bool) -> Option<Token> {
        match self.peek() {
            Some(kind) if f(kind) => self.next(),
            _ => None,
        }
    }

    fn consume(&mut self, expected: TokenKind, error: ParseError) -> Result<Token> {
        match self.next_if(|kind| *kind == expected) {
            Some(token) => Ok(token),
            None => Err(error),
        }
    }

    fn declaration(&mut self) -> Result<Stmt> {
        let stmt = match self.next_if(|kind| matches!(kind, TokenKind::Var)) {
            Some(_) => self.var_declaration(),
            None => self.statement(),
        };

        if stmt.is_err() {
            self.synchronize();
        }

        stmt
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let token = self.consume(TokenKind::Identifier, ParseError::ExpectVariableName)?;

        let name = token.lexeme.to_owned();

        let initalizer = match self.next_if(|kind| matches!(kind, TokenKind::Equal)) {
            Some(_) => Some(self.expression()?),
            None => None,
        };

        self.consume(TokenKind::Semicolon, ParseError::ExpectSemiAfterVarDecl)?;

        Ok(Stmt::Var(Var::new(name, initalizer)))
    }

    fn statement(&mut self) -> Result<Stmt> {
        // TODO: make cleaner?
        match self
            .next_if(|kind| {
                matches!(
                    kind,
                    TokenKind::Print
                        | TokenKind::LeftBrace
                        | TokenKind::If
                        | TokenKind::While
                        | TokenKind::For
                )
            })
            .map(|token| token.kind)
        {
            Some(TokenKind::Print) => self.print_statement(),
            Some(TokenKind::LeftBrace) => self.block(),
            Some(TokenKind::If) => self.if_statement(),
            Some(TokenKind::While) => self.while_statement(),
            Some(TokenKind::For) => self.for_statement(),
            None => self.expression_statement(),
            _ => unreachable!(),
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;

        self.consume(TokenKind::Semicolon, ParseError::ExpectSemiAfterValue)?;

        Ok(Stmt::Print(expr))
    }

    fn block(&mut self) -> Result<Stmt> {
        Ok(Stmt::Block(self.statements()?))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LeftParen, ParseError::ExpectLeftParenAfterIf)?;

        let condition = self.expression()?;

        self.consume(
            TokenKind::RightParen,
            ParseError::ExpectRightParenAfterIfCond,
        )?;

        let then_stmt = self.statement()?;

        let else_stmt = match self.next_if(|kind| matches!(kind, TokenKind::Else)) {
            Some(_) => Some(self.statement()?),
            None => None,
        };

        Ok(Stmt::If(Box::new(If::new(condition, then_stmt, else_stmt))))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LeftParen, ParseError::ExpectLeftParenAfterWhile)?;

        let condition = self.expression()?;

        self.consume(
            TokenKind::RightParen,
            ParseError::ExpectRightParenAfterWhile,
        )?;

        let body = self.statement()?;

        Ok(Stmt::While(Box::new(While::new(condition, body))))
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(TokenKind::LeftParen, ParseError::ExpectParenAfterFor)?;

        let initializer = match self
            .next_if(|kind| matches!(kind, TokenKind::Semicolon | TokenKind::Var))
            .map(|token| token.kind)
        {
            Some(TokenKind::Semicolon) => None,
            Some(TokenKind::Var) => Some(self.var_declaration()?),
            None => Some(self.expression_statement()?),
            _ => unreachable!(),
        };

        let condition = match self.next_if(|kind| matches!(kind, TokenKind::Semicolon)) {
            Some(_) => None,
            None => Some(self.expression()?),
        };

        self.consume(
            TokenKind::Semicolon,
            ParseError::ExpectSemicolonAfterLoopCond,
        )?;

        let increment = match self.next_if(|kind| matches!(kind, TokenKind::Semicolon)) {
            Some(_) => None,
            None => Some(self.expression()?),
        };

        self.consume(TokenKind::RightParen, ParseError::ExpectLeftParenAfterFor)?;

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

        self.consume(TokenKind::Semicolon, ParseError::ExpectSemiAfterExpression)?;

        Ok(Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr> {
        let expr = self.or()?;

        if self
            .next_if(|kind| matches!(kind, TokenKind::Equal))
            .is_some()
        {
            let value = self.assignment()?;

            if let Expr::Variable(name) = expr {
                Ok(Expr::Assign(Box::new(Assign::new(name, value))))
            } else {
                Err(ParseError::InvalidAssignmentTarget)
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;

        while let Some(token) = self.next_if(|kind| matches!(kind, TokenKind::Or)) {
            let kind = token.kind.into();
            let right = self.and()?;

            expr = Expr::Logical(Box::new(Logical::new(expr, kind, right)));
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while let Some(token) = self.next_if(|kind| matches!(kind, TokenKind::And)) {
            let kind = token.kind.into();
            let right = self.equality()?;

            expr = Expr::Logical(Box::new(Logical::new(expr, kind, right)));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut expr = self.comparison()?;

        while let Some(token) =
            self.next_if(|kind| matches!(kind, TokenKind::BangEqual | TokenKind::EqualEqual))
        {
            let operator = token.kind.into();
            let right = self.comparison()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr> {
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

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut expr = self.factor()?;

        while let Some(token) =
            self.next_if(|kind| matches!(kind, TokenKind::Minus | TokenKind::Plus))
        {
            let operator = token.kind.into();
            let right = self.factor()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut expr = self.unary()?;

        while let Some(token) =
            self.next_if(|kind| matches!(kind, TokenKind::Slash | TokenKind::Star))
        {
            let operator = token.kind.into();
            let right = self.unary()?;

            expr = Expr::Binary(Box::new(Binary::new(expr, operator, right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some(token) = self.next_if(|kind| matches!(kind, TokenKind::Bang | TokenKind::Minus))
        {
            let operator = token.kind.into();
            let right = self.unary()?;

            Ok(Expr::Unary(Box::new(Unary::new(operator, right))))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr> {
        if let Some(token) = self.next_if(|kind| matches!(kind, TokenKind::Literal(_))) {
            let TokenKind::Literal(literal) = token.kind else {
                unreachable!()
            };

            return Ok(Expr::Literal(literal));
        }

        if let Some(token) = self.next_if(|kind| matches!(kind, TokenKind::Identifier)) {
            return Ok(Expr::Variable(token.lexeme.to_owned()));
        }

        self.consume(TokenKind::LeftParen, ParseError::ExpectExpression)?;

        let expr = self.expression()?;

        self.consume(
            TokenKind::RightParen,
            ParseError::ExpectParenAfterExpression,
        )?;

        Ok(Expr::Grouping(Box::new(Grouping::new(expr))))
    }

    fn synchronize(&mut self) {
        while let Some(kind) = self.peek() {
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

    fn statements(&mut self) -> Result<Vec<Stmt>> {
        let mut statements = vec![];

        while self
            .peek()
            .is_some_and(|kind| !matches!(kind, TokenKind::RightBrace))
        {
            statements.push(self.declaration()?);
        }

        self.consume(TokenKind::RightBrace, ParseError::ExpectRightBrace)?;

        Ok(statements)
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.declaration() {
            Ok(stmt) => Some(Ok(stmt)),
            Err(ParseError::ExpectExpression) => None,
            Err(err) => Some(Err(err)),
        }
    }
}
