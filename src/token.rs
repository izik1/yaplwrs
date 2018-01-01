#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub loc: super::util::Loc,
    pub token_type: TokenType,
}

impl Token {
    pub fn require(&self, expected: &TokenType) -> Result<&Token, ::error::CompilerError> {
        match (&self.token_type, expected) {
            (&TokenType::Integer(_), &TokenType::Integer(_))
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
    Integer(String),
    Keyword(Keyword),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Keyword {
    Const,
    Function,
    If,
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
