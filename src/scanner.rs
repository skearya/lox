use crate::token::{Literal, Token, TokenKind};

#[derive(Debug)]
pub struct Scanner<'src> {
    source: &'src str,
    start: usize,
    current: usize,
    line: usize,
}

// TODO: don't panic on error

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.current..)?.chars().next()
    }

    fn peek_peek(&self) -> Option<char> {
        self.source.get(self.current + 1..)?.chars().next()
    }

    fn next(&mut self) -> Option<char> {
        self.peek()
            .inspect(|c| self.current += c.len_utf8())
            .inspect(|&c| {
                if c == '\n' {
                    self.line += 1;
                }
            })
    }

    fn next_if_eq(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.next();
            true
        } else {
            false
        }
    }

    fn token(&mut self) -> Option<Token<'src>> {
        self.start = self.current;

        let kind = match self.next()? {
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Star,
            '!' if self.next_if_eq('=') => TokenKind::BangEqual,
            '!' => TokenKind::Bang,
            '=' if self.next_if_eq('=') => TokenKind::EqualEqual,
            '=' => TokenKind::Equal,
            '<' if self.next_if_eq('=') => TokenKind::LessEqual,
            '<' => TokenKind::Less,
            '>' if self.next_if_eq('=') => TokenKind::GreaterEqual,
            '>' => TokenKind::Greater,
            '/' if self.next_if_eq('/') => {
                while self.next()? != '\n' {}
                return None;
            }
            '/' => TokenKind::Slash,
            ' ' | '\r' | '\t' | '\n' => return None,
            '"' => return Some(self.string()),
            '0'..='9' => return Some(self.number()),
            'a'..='z' | 'A'..='Z' | '_' => return Some(self.identifier()),
            unknown => {
                panic!("unexpected character: {unknown}");
            }
        };

        let token = Token {
            kind,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        };

        Some(token)
    }

    fn string(&mut self) -> Token<'src> {
        loop {
            match self.next() {
                Some('"') => break,
                Some(_) => {}
                None => panic!("unterminated string"),
            }
        }

        let data = self.source[self.start + 1..self.current - 1].to_owned();

        Token {
            kind: TokenKind::Literal(Literal::String(data)),
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn number(&mut self) -> Token<'src> {
        while matches!(self.peek(), Some('0'..='9')) {
            self.next();
        }

        // Look for a fractional part
        if self.peek() == Some('.') && matches!(self.peek_peek(), Some('0'..='9')) {
            self.next();

            while matches!(self.peek(), Some('0'..='9')) {
                self.next();
            }
        }

        let data = self.source[self.start..self.current]
            .parse::<f64>()
            .map(Literal::Number)
            .expect("expected parseable float");

        Token {
            kind: TokenKind::Literal(data),
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn identifier(&mut self) -> Token<'src> {
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '_')) {
            self.next();
        }

        let lexeme = &self.source[self.start..self.current];
        let kind = Scanner::keyword(lexeme).unwrap_or(TokenKind::Identifier);

        Token {
            kind,
            lexeme,
            line: self.line,
        }
    }

    fn keyword(str: &str) -> Option<TokenKind> {
        match str {
            "and" => Some(TokenKind::And),
            "class" => Some(TokenKind::Class),
            "else" => Some(TokenKind::Else),
            "for" => Some(TokenKind::For),
            "fun" => Some(TokenKind::Fun),
            "if" => Some(TokenKind::If),
            "or" => Some(TokenKind::Or),
            "print" => Some(TokenKind::Print),
            "return" => Some(TokenKind::Return),
            "super" => Some(TokenKind::Super),
            "this" => Some(TokenKind::This),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            "true" => Some(TokenKind::Literal(Literal::Bool(true))),
            "false" => Some(TokenKind::Literal(Literal::Bool(false))),
            "nil" => Some(TokenKind::Literal(Literal::Nil)),
            _ => None,
        }
    }
}

impl<'src> Iterator for Scanner<'src> {
    type Item = Token<'src>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current < self.source.len() {
                if let Some(token) = self.token() {
                    break Some(token);
                }
            } else {
                break None;
            }
        }
    }
}
