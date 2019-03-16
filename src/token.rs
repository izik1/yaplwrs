use super::util;
use error::{self, SpanError};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: util::Span,
    pub token_type: TokenType,
}

impl Token {
    pub fn require(&self, expected: &TokenType) -> Result<&Token, error::SpanError> {
        if self.matches(expected) {
            Ok(self)
        } else {
            Err(SpanError::new("Unexpected token".to_string(), self.span))
        }
    }

    pub fn matches(&self, expected: &TokenType) -> bool {
        match (&self.token_type, expected) {
            (&TokenType::Integer(_, _), &TokenType::Integer(_, _))
            | (&TokenType::Ident(_), &TokenType::Ident(_)) => true,
            _ => &self.token_type == expected,
        }
    }

    pub fn new(span: util::Span, token_type: TokenType) -> Token {
        Token { span, token_type }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Grammar(Grammar),
    Ident(String),
    Integer(String, Option<String>),
    Keyword(Keyword),
    Err(String),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenType::Grammar(ref g) => write!(f, "{}", g),
            TokenType::Ident(ref id) => write!(f, "{}", id),
            TokenType::Integer(ref i, Some(ref ty)) if ty != "i32" => write!(f, "{}_{}", i, ty),
            TokenType::Integer(ref i, _) => write!(f, "{}", i),
            TokenType::Keyword(ref k) => write!(f, "{}", k),
            TokenType::Err(ref s) => write!(f, "{}", s),
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
