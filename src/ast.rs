use std::fmt;
use std::fmt::Display;

#[derive(Debug, Eq, PartialEq)]
pub struct Ident(pub String);

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IDENT[{}]", self.0)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct ScopedBlock(pub Box<[Node]>, pub Option<Box<Expr>>);

impl Display for ScopedBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SCOPEDBLOCK[STMTS[")?;

        for node in self.0.iter() {
            write!(f, "{},", node)?;
        }

        write!(f, "],IMPLICITRET[")?;

        if let Some(implicit_return) = &self.1 {
            write!(f, "SOME[{}]]", implicit_return)?;
        } else {
            write!(f, "NONE]")?;
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
        Self { header, body }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "FUNCTION[{},{}]", self.header, self.body)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionArg {
    pub ident: Ident,
    pub ty: Ident,
}

impl Display for FunctionArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ARG[NAME[{}],TYPE[{}]]", self.ident, self.ty)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionHeader {
    pub identifier: Ident,
    pub args: Box<[FunctionArg]>,
    pub ret_type: Option<Ident>,
}

impl Display for FunctionHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "HEADER[{},ARGS[", self.identifier)?;

        for arg in &*self.args {
            write!(f, "{},", arg)?;
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
    pub fn new(identifier: Ident, args: Box<[FunctionArg]>, ret_type: Option<Ident>) -> Self {
        Self {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ELSEIF[COND[{}],BLOCK[{}]]", self.0, self.1)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Primary {
    If(If),
    Integer(String, Option<String>),
    Ident(Ident),
    FunctionCall(Ident, Box<[Expr]>),
    ScopedBlock(ScopedBlock)
}

impl Display for Primary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            },
            Primary::ScopedBlock(block) => write!(f, "{}", block)?,
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    Add,
    Sub,
    Bin,
    Div,
}

impl fmt::Display for BinOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                BinOperator::Add => "ADD",
                BinOperator::Sub => "SUBTRACT",
                BinOperator::Bin => "MULTIPLY",
                BinOperator::Div => "DIVIDE",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum UnaryOperator {
    UnNeg,
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    pub fn from_token_type(ty: &crate::token::Kind) -> Option<Self> {
        use crate::token::{Grammar, Kind};
        match *ty {
            Kind::Grammar(Grammar::Minus) => Some(UnaryOperator::UnNeg),
            _ => None,
        }
    }
}

impl BinOperator {
    pub fn from_token_type(ty: &crate::token::Kind) -> Option<Self> {
        use crate::token::{Grammar, Kind};
        match *ty {
            Kind::Grammar(ref g) => match *g {
                Grammar::Plus => Some(BinOperator::Add),
                Grammar::Slash => Some(BinOperator::Div),
                Grammar::Minus => Some(BinOperator::Sub),
                Grammar::Star => Some(BinOperator::Bin),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn precedence(self) -> usize {
        match self {
            BinOperator::Add | BinOperator::Sub => 150,
            BinOperator::Bin | BinOperator::Div => 200,
        }
    }

    pub fn associativity(self) -> Associativity {
        match self {
            BinOperator::Add | BinOperator::Sub | BinOperator::Bin | BinOperator::Div => {
                Associativity::Left
            }
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
pub enum Node {
    Mod(Box<[Node]>),
    Function(Function),
    ScopedBlock(ScopedBlock),
    Expr(Expr),
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ASTNODE[")?;
        match self {
            Node::Mod(nodes) => {
                write!(f, "MOD[")?;
                for node in nodes.iter() {
                    write!(f, "{},", node)?;
                }

                write!(f, "]")?
            }
            Node::Function(func) => write!(f, "{}", func)?,
            Node::ScopedBlock(block) => write!(f, "{}", block)?,
            Node::Expr(expr) => write!(f, "{}", expr)?,
        };

        write!(f, "]")
    }
}
