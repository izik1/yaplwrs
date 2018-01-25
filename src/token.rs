use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub loc: super::util::Loc,
    pub token_type: TokenType,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ({})", self.token_type, self.loc)
    }
}

impl Token {
    pub fn require(&self, expected: &TokenType) -> Result<&Token, ::error::CompilerError> {
        match (&self.token_type, expected) {
            (&TokenType::Integer(_, _), &TokenType::Integer(_, _))
            | (&TokenType::Identifier(_), &TokenType::Identifier(_)) => Ok(self),
            _ => if &self.token_type == expected {
                Ok(self)
            } else {
                Err(::error::CompilerError::with_loc(
                    "Unexpected token",
                    self.loc,
                ))
            },
        }
    }

    pub fn new(loc: ::util::Loc, token_type: TokenType) -> Token {
        Token { loc, token_type }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Grammar(Grammar),
    Identifier(String),
    Integer(String, String),
    Keyword(Keyword),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Grammar(ref g) => write!(f, "{}", g),
            TokenType::Identifier(ref id) => write!(f, "{}", id),
            TokenType::Integer(ref i, ref ty) if ty != "i32" => write!(f, "{}_{}", i, ty),
            TokenType::Integer(ref i, _) => write!(f, "{}", i),
            TokenType::Keyword(ref k) => write!(f, "{}", k),
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Keyword {
    Const,
    Function,
    If,
    Else,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Keyword::Const => "const",
                Keyword::Function => "fn",
                Keyword::If => "if",
                Keyword::Else => "else",
            }
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum Grammar {
    Arrow,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Plus,
    Minus,
    Star,
    Slash,
    SemiColon,
    Colon,
    Comma,
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Grammar::Arrow => "->",
                Grammar::OpenParen => "(",
                Grammar::CloseParen => ")",
                Grammar::OpenBrace => "{",
                Grammar::CloseBrace => "}",
                Grammar::Plus => "+",
                Grammar::Minus => "-",
                Grammar::Star => "*",
                Grammar::Slash => "/",
                Grammar::SemiColon => ";",
                Grammar::Colon => ":",
                Grammar::Comma => ",",
            }
        )
    }
}
