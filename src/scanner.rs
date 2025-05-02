use crate::token::{Literal, Token, TokenKind};

#[derive(Debug)]
pub struct Scanner<'src> {
    source: &'src str,
    start: usize,
    current: usize,
    line: usize,
}

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

    fn peekpeek(&self) -> Option<char> {
        self.source.get(self.current + 1..)?.chars().next()
    }

    fn next(&mut self) -> Option<char> {
        let next = self.peek()?;

        if next == '\n' {
            self.line += 1;
        }

        self.current += next.len_utf8();

        Some(next)
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
            '"' => self.string()?,
            '0'..='9' => self.number()?,
            'a'..='z' | 'A'..='Z' | '_' => self.identifier()?,
            unexpected => {
                eprintln!("Unexpected character: {unexpected}");
                return None;
            }
        };

        let token = Token {
            kind,
            lexeme: &self.source[self.start..self.current],
            line: self.line,
        };

        Some(token)
    }

    fn string(&mut self) -> Option<TokenKind> {
        loop {
            match self.next() {
                Some('"') => break,
                Some(_) => continue,
                None => {
                    eprintln!("Unterminated string");
                    return None;
                }
            }
        }

        let data = self.source[self.start + 1..self.current - 1].to_owned();

        Some(TokenKind::Literal(Literal::String(data)))
    }

    fn number(&mut self) -> Option<TokenKind> {
        while matches!(self.peek(), Some('0'..='9')) {
            self.next();
        }

        if self.peek() == Some('.') && matches!(self.peekpeek(), Some('0'..='9')) {
            self.next();

            while matches!(self.peek(), Some('0'..='9')) {
                self.next();
            }
        }

        let data = self.source[self.start..self.current]
            .parse()
            .map(Literal::Number)
            .inspect_err(|err| eprintln!("Error parsing float: {err:#?}"))
            .ok()?;

        Some(TokenKind::Literal(data))
    }

    fn identifier(&mut self) -> Option<TokenKind> {
        while matches!(self.peek(), Some('a'..='z' | 'A'..='Z' | '_')) {
            self.next();
        }

        let kind = Scanner::keyword(&self.source[self.start..self.current])
            .unwrap_or(TokenKind::Identifier);

        Some(kind)
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
