use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
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
                    self.line += 1
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

    fn token(&mut self) -> Option<Token<'a>> {
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

    pub fn tokens(&mut self) -> Vec<Token<'a>> {
        let mut tokens = vec![];

        while self.current < self.source.len() {
            if let Some(token) = self.token() {
                tokens.push(token);
            }
        }

        tokens
    }

    fn string(&mut self) -> Token<'a> {
        loop {
            match self.next() {
                Some('"') => break,
                Some(_) => {}
                None => panic!("unterminated string"),
            }
        }

        let data = self.source[self.start + 1..self.current - 1].to_owned();

        Token {
            kind: TokenKind::String(data),
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn number(&mut self) -> Token<'a> {
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
            .expect("expected parseable float");

        Token {
            kind: TokenKind::Number(data),
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        }
    }

    fn identifier(&mut self) -> Token<'a> {
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '_')) {
            self.next();
        }

        let lexeme = &self.source[self.start..self.current];
        let kind = Scanner::to_keyword(lexeme).unwrap_or(TokenKind::Identifier);

        Token {
            kind,
            lexeme,
            line: self.line,
        }
    }

    fn to_keyword(text: &str) -> Option<TokenKind> {
        match text {
            "and" => Some(TokenKind::And),
            "class" => Some(TokenKind::Class),
            "else" => Some(TokenKind::Else),
            "false" => Some(TokenKind::False),
            "for" => Some(TokenKind::For),
            "fun" => Some(TokenKind::Fun),
            "if" => Some(TokenKind::If),
            "nil" => Some(TokenKind::Nil),
            "or" => Some(TokenKind::Or),
            "print" => Some(TokenKind::Print),
            "return" => Some(TokenKind::Return),
            "super" => Some(TokenKind::Super),
            "this" => Some(TokenKind::This),
            "true" => Some(TokenKind::True),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            _ => None,
        }
    }
}
