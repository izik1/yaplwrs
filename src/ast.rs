use std::fmt;
use std::fmt::Display;

#[derive(Debug, Eq, PartialEq)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IDENT[{}]", self.0)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ScopedBlock(pub Box<[AstNode]>);

impl Display for ScopedBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SCOPEDBLOCK[")?;

        for node in self.0.iter() {
            write!(f, "{},", node)?;
        }

        write!(f, "]")
    }
}

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

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FUNCTION[{},{}]", self.header, self.body)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionHeader {
    pub identifier: Ident,
    pub args: Box<[(Ident, Ident)]>,
    pub ret_type: Option<Ident>,
}

impl Display for FunctionHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "HEADER[{},ARGS[", self.identifier)?;

        for (name, ty) in self.args.iter() {
            write!(f, "ARG[NAME[{}],TYPE[{}]],", name, ty)?;
        }

        write!(f, "],RET[")?;

        match self.ret_type.as_ref() {
            Some(id) => write!(f, "{}", id)?,
            None => write!(f, "()")?,
        };

        write!(f, "]]")
    }
}

impl FunctionHeader {
    pub fn new(
        identifier: Ident,
        args: Box<[(Ident, Ident)]>,
        ret_type: Option<Ident>,
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
    pub Box<[ElseIf]>,
    pub Option<ScopedBlock>,
);

impl Display for If {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "IF[COND[{}],BLOCK[{}],ELSEIFS[", self.0, self.1)?;
        for elif in self.2.iter() {
            write!(f, "{},", elif)?;
        }

        write!(f, "],")?;

        if let Some(block) = self.3.as_ref() {
            write!(f, "ELSE[{}]", block)?;
        }

        write!(f, "]")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ElseIf(pub Box<Expr>, pub ScopedBlock);

impl Display for ElseIf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ELSEIF[COND[{}],BLOCK[{}]]", self.0, self.1)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Primary {
    If(If),
    Integer(String, Option<String>),
    Ident(Ident),
    FunctionCall(Ident, Box<[Expr]>),
}

impl Display for Primary {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PRIMARY[")?;

        match self {
            Primary::If(e) => write!(f, "{}", e)?,
            Primary::Integer(number, Some(base)) => write!(f, "INTEGER[{}_{}]", number, base)?,
            Primary::Integer(number, None) => write!(f, "INTEGER[{}]", number)?,
            Primary::Ident(id) => write!(f, "{}", id)?,
            Primary::FunctionCall(name, params) => {
                write!(f, "CALL[NAME[{}],PARAMS[", name)?;
                for param in params.iter() {
                    write!(f, "{},", param)?;
                }

                write!(f, "]]")?
            }
        }

        write!(f, "]")
    }
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
                BinOperator::BinAdd => "ADD",
                BinOperator::BinSub => "SUBTRACT",
                BinOperator::BinMul => "MULTIPLY",
                BinOperator::BinDiv => "DIVIDE",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryOperator {
    UnNeg,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::UnNeg => "NEGATE",
            }
        )
    }
}

impl UnaryOperator {
    pub fn from_token_type(ty: &crate::token::TokenType) -> Option<Self> {
        use crate::token::{Grammar, TokenType};
        match *ty {
            TokenType::Grammar(Grammar::Minus) => Some(UnaryOperator::UnNeg),
            _ => None,
        }
    }
}

impl BinOperator {
    pub fn from_token_type(ty: &crate::token::TokenType) -> Option<Self> {
        use crate::token::{Grammar, TokenType};
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

    pub fn precedence(self) -> usize {
        match self {
            BinOperator::BinAdd | BinOperator::BinSub => 150,
            BinOperator::BinMul | BinOperator::BinDiv => 200,
        }
    }

    pub fn associativity(self) -> Associativity {
        match self {
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

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EXPR[")?;

        match self {
            Expr::Binary(op, lhs, rhs) => write!(f, "OP[{}],LHS[{}],RHS[{}]", op, lhs, rhs)?,

            Expr::Unary(op, expr) => write!(f, "OP[{}],SUB[{}]", op, expr)?,

            Expr::Primary(primary) => write!(f, "{}", primary)?,
        };

        write!(f, "]")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AstNode {
    Token(crate::token::Token),
    Mod(Box<[AstNode]>),
    Function(Function),
    ScopedBlock(ScopedBlock),
    Expr(Expr),
}

impl Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ASTNODE[")?;
        match self {
            AstNode::Token(token) => write!(f, "{}", token)?,
            AstNode::Mod(nodes) => {
                write!(f, "MOD[")?;
                for node in nodes.iter() {
                    write!(f, "{},", node)?;
                }

                write!(f, "]")?
            }
            AstNode::Function(func) => write!(f, "{}", func)?,
            AstNode::ScopedBlock(block) => write!(f, "{}", block)?,
            AstNode::Expr(expr) => write!(f, "{}", expr)?,
        };

        write!(f, "]")
    }
}
