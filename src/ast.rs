#[derive(Debug, Eq, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug, Eq, PartialEq)]
pub struct ScopedBlock(pub Vec<AstNode>);

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionHeader(
    Identifier,
    Vec<(Identifier, Identifier)>,
    Option<Identifier>,
);

#[derive(Debug, Eq, PartialEq)]
pub struct If(pub Box<Expr>, pub ScopedBlock, pub Option<ScopedBlock>);

#[derive(Debug, Eq, PartialEq)]
pub enum Primary {
    If(If),
    Integer(String),
    Identifier(Identifier),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Associativity {
    Right,
    Left,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinOperator {
    Plus,
    Minus,
    Star,
    Slash,
}

impl BinOperator {
    pub fn from_token_type(ty: &::token::TokenType) -> Option<Self> {
        use token::{Grammar, TokenType};
        match *ty {
            TokenType::Grammar(ref g) => match *g {
                Grammar::Plus => Some(BinOperator::Plus),
                Grammar::Slash => Some(BinOperator::Slash),
                Grammar::Minus => Some(BinOperator::Minus),
                Grammar::Star => Some(BinOperator::Star),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn precedence(&self) -> usize {
        match *self {
            BinOperator::Plus | BinOperator::Minus => 150,
            BinOperator::Star | BinOperator::Slash => 200,
        }
    }

    pub fn associativity(&self) -> Associativity {
        match *self {
            BinOperator::Plus | BinOperator::Minus | BinOperator::Star | BinOperator::Slash => {
                Associativity::Left
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Binary(BinOperator, Box<Expr>, Box<Expr>),
    Unary(Box<Expr>),
    Primary(Primary),
}

impl Expr {
    pub fn unbox(value: Box<Self>) -> Self {
        *value
    }
}

impl FunctionHeader {
    pub fn new(
        name: Identifier,
        args: Vec<(Identifier, Identifier)>,
        return_type: Option<Identifier>,
    ) -> Self {
        FunctionHeader(name, args, return_type)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AstNode {
    Token(::token::Token),
    Mod(Vec<AstNode>),
    Function(FunctionHeader, ScopedBlock),
    ScopedBlock(ScopedBlock),
    Expr(Expr),
}
