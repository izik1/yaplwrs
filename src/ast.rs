use std::fmt;

#[derive(Debug, Eq, PartialEq)]
pub struct Identifier(pub String);

#[derive(Debug, Eq, PartialEq)]
pub struct ScopedBlock(pub Vec<AstNode>);

#[derive(Debug, Eq, PartialEq)]
pub struct Function {
    pub header: FunctionHeader,
    pub body: ScopedBlock,
}

impl Function {
    pub fn new(header: FunctionHeader, body: ScopedBlock) -> Self {
        Function { header, body }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionHeader {
    pub identifier: Identifier,
    pub args: Vec<(Identifier, Identifier)>,
    pub ret_type: Option<Identifier>,
}

impl FunctionHeader {
    pub fn new(
        identifier: Identifier,
        args: Vec<(Identifier, Identifier)>,
        ret_type: Option<Identifier>,
    ) -> FunctionHeader {
        FunctionHeader {
            identifier,
            args,
            ret_type,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct If(
    pub Box<Expr>,
    pub ScopedBlock,
    pub Vec<ElseIf>,
    pub Option<ScopedBlock>,
);

#[derive(Debug, Eq, PartialEq)]
pub struct ElseIf(pub Box<Expr>, pub ScopedBlock);

#[derive(Debug, Eq, PartialEq)]
pub enum Primary {
    If(If),
    Integer(String, String),
    Identifier(Identifier),
    FunctionCall(Identifier, Vec<Expr>),
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Associativity {
    Right,
    Left,
}

impl fmt::Display for Associativity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Associativity::Left => "Left",
                Associativity::Right => "Right",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BinOperator {
    BinAdd,
    BinSub,
    BinMul,
    BinDiv,
}

impl fmt::Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                BinOperator::BinAdd => "+",
                BinOperator::BinSub => "-",
                BinOperator::BinMul => "*",
                BinOperator::BinDiv => "/",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryOperator {
    UnNeg,
}

impl UnaryOperator {
    pub fn from_token_type(ty: &::token::TokenType) -> Option<Self> {
        use token::{Grammar, TokenType};
        match *ty {
            TokenType::Grammar(Grammar::Minus) => Some(UnaryOperator::UnNeg),
            _ => None,
        }
    }
}

impl BinOperator {
    pub fn from_token_type(ty: &::token::TokenType) -> Option<Self> {
        use token::{Grammar, TokenType};
        match *ty {
            TokenType::Grammar(ref g) => match *g {
                Grammar::Plus => Some(BinOperator::BinAdd),
                Grammar::Slash => Some(BinOperator::BinDiv),
                Grammar::Minus => Some(BinOperator::BinSub),
                Grammar::Star => Some(BinOperator::BinMul),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn precedence(&self) -> usize {
        match *self {
            BinOperator::BinAdd | BinOperator::BinSub => 150,
            BinOperator::BinMul | BinOperator::BinDiv => 200,
        }
    }

    pub fn associativity(&self) -> Associativity {
        match *self {
            BinOperator::BinAdd
            | BinOperator::BinSub
            | BinOperator::BinMul
            | BinOperator::BinDiv => Associativity::Left,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Expr {
    Binary(BinOperator, Box<Expr>, Box<Expr>),
    Unary(UnaryOperator, Box<Expr>),
    Primary(Primary),
}

impl Expr {
    pub fn unbox(value: Box<Self>) -> Self {
        *value
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AstNode {
    Token(::token::Token),
    Mod(Vec<AstNode>),
    Function(Function),
    ScopedBlock(ScopedBlock),
    Expr(Expr),
}
