#[derive(Debug, Clone)]
pub enum Keyword {
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
}

impl Keyword {
    pub fn from_str(text: &str) -> Option<Keyword> {
        match text {
            "and" => Some(Keyword::And),
            "class" => Some(Keyword::Class),
            "else" => Some(Keyword::Else),
            "false" => Some(Keyword::False),
            "for" => Some(Keyword::For),
            "fun" => Some(Keyword::Fun),
            "if" => Some(Keyword::If),
            "nil" => Some(Keyword::Nil),
            "or" => Some(Keyword::Or),
            "print" => Some(Keyword::Print),
            "return" => Some(Keyword::Return),
            "super" => Some(Keyword::Super),
            "this" => Some(Keyword::This),
            "true" => Some(Keyword::True),
            "var" => Some(Keyword::Var),
            "while" => Some(Keyword::While),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Keyword::And => "and",
            Keyword::Class => "class",
            Keyword::Else => "else",
            Keyword::False => "false",
            Keyword::Fun => "fun",
            Keyword::For => "for",
            Keyword::If => "if",
            Keyword::Nil => "nil",
            Keyword::Or => "or",
            Keyword::Print => "print",
            Keyword::Return => "return",
            Keyword::Super => "super",
            Keyword::This => "this",
            Keyword::True => "true",
            Keyword::Var => "var",
            Keyword::While => "while",
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Keyword(Keyword),
    Identifier(String),
    String(String),
    Number(f64),
}

#[derive(Debug)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Literal(Literal),
}

// TODO: Reduce size
#[derive(Debug)]
pub struct Token<'src> {
    pub kind: TokenKind,
    pub lexeme: &'src str,
    pub line: usize,
}
