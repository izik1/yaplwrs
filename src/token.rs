use super::util;
use crate::error::{self, Span};
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub span: util::Span,
    pub token_type: Kind,
}

impl Token {
    pub fn require(&self, expected: &Kind) -> Result<&Self, error::Span> {
        if self.matches(expected) {
            Ok(self)
        } else {
            Err(Span::new("Unexpected token".to_string(), self.span))
        }
    }

    pub fn matches(&self, expected: &Kind) -> bool {
        match (&self.token_type, expected) {
            (&Kind::Integer(_, _), &Kind::Integer(_, _)) | (&Kind::Ident(_), &Kind::Ident(_)) => {
                true
            }
            _ => &self.token_type == expected,
        }
    }

    pub fn new(span: util::Span, token_type: Kind) -> Self {
        Self { span, token_type }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TOKEN[{}@{}]", self.token_type, self.span)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Kind {
    Grammar(Grammar),
    Ident(String),
    Integer(String, Option<String>),
    Keyword(Keyword),
    Err(String),
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Kind::Grammar(ref g) => write!(f, "GRAMMAR[{}]", g),
            Kind::Ident(ref id) => write!(f, "IDENT[{}]", id),
            Kind::Integer(ref i, Some(ref ty)) if ty != "i32" => write!(f, "INTEGER[{}_{}]", i, ty),
            Kind::Integer(ref i, _) => write!(f, "INTEGER[{}]", i),
            Kind::Keyword(ref k) => write!(f, "KEYWORD[{}]", k),
            Kind::Err(ref s) => write!(f, "ERROR[{}]", s),
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Keyword::Const => "CONST",
                Keyword::Function => "FN",
                Keyword::If => "IF",
                Keyword::Else => "ELSE",
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Grammar::Arrow => "ARROW",
                Grammar::OpenParen => "LEFT_PAREN",
                Grammar::CloseParen => "RIGHT_PAREN",
                Grammar::OpenBrace => "LEFT_BRACKET",
                Grammar::CloseBrace => "RIGHT_BRACKET",
                Grammar::Plus => "PLUS",
                Grammar::Minus => "MINUS",
                Grammar::Star => "STAR",
                Grammar::Slash => "SLASH",
                Grammar::SemiColon => "SEMICOLON",
                Grammar::Colon => "COLON",
                Grammar::Comma => "COMMA",
            }
        )
    }
}
