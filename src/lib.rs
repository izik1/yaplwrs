mod lexer;
mod parser;
pub mod error;

mod ast {
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
        Integer(u64),
        Identifier(Identifier),
    }

    #[derive(Debug, Eq, PartialEq)]
    pub enum Expr {
        Binary(Box<Expr>, Box<Expr>),
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
}

pub mod token {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Token {
        pub loc: super::util::Loc,
        pub token_type: TokenType,
    }

    impl Token {
        pub fn require(&self, expected: &TokenType) -> Result<&Token, ::error::CompilerError> {
            match (&self.token_type, expected) {
                (&TokenType::Integer(_), &TokenType::Integer(_))
                | (&TokenType::Const, &TokenType::Const)
                | (&TokenType::Identifier(_), &TokenType::Identifier(_)) => Ok(self),
                (&TokenType::Grammar(grammar), &TokenType::Grammar(expected_grammar))
                    if grammar == expected_grammar =>
                {
                    Ok(self)
                }
                _ => Err(::error::CompilerError::with_loc(
                    "Unexpected token",
                    self.loc,
                )),
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
        Integer(u64),
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
    }
}

pub mod util {
    use std::fmt;

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    pub struct Loc {
        pub ln: usize,
        pub col: usize,
    }

    impl Loc {
        pub fn new(ln: usize, col: usize) -> Self {
            Loc { ln, col }
        }

        pub fn from_tuple(pos: (usize, usize)) -> Loc {
            Loc {
                ln: pos.0,
                col: pos.1,
            }
        }

        pub fn from_string(string: &str, index: usize) -> Loc {
            Self::from_tuple(str_index_ln_col(string, index).expect("Failed to create Loc."))
        }
    }

    impl fmt::Display for Loc {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} {}", self.ln, self.col)
        }
    }

    pub fn str_index_ln_col(string: &str, index: usize) -> Result<(usize, usize), &'static str> {
        if string.chars().count() <= index {
            Err("Index out of range of string")
        } else if string.chars().nth(index) == Some('\n') {
            Err("Trying to get line / column of unprintable character")
        } else {
            let mut ln = 1;
            let mut column = 1;
            for ch in string.chars().take(index) {
                if ch == '\n' {
                    ln += 1;
                    column = 0;
                }

                column += 1;
            }

            Ok((ln, column))
        }
    }
}

pub fn lex(string: &str) -> Result<Vec<token::Token>, error::CompilerError> {
    lexer::Lexer::new(string).lex_all()
}

pub fn parse(string: &str) -> Result<ast::AstNode, error::CompilerError> {
    parser::parse(&lex(string)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse {
        use super::*;
        use ast::*;
        #[test]
        fn empty() {
            assert_eq!(parse("").unwrap(), ast::AstNode::Mod(vec![]))
        }

        #[test]
        fn scope() {
            assert_eq!(
                parse("fn foo() {{}}").unwrap(),
                AstNode::Mod(vec![
                    AstNode::Function(
                        FunctionHeader::new(Identifier("foo".to_string()), vec![], None),
                        ScopedBlock(vec![AstNode::ScopedBlock(ScopedBlock(vec![]))]),
                    ),
                ])
            )
        }

        #[test]
        fn empty_fn() {
            assert_eq!(
                parse("fn foo() {}").unwrap(),
                AstNode::Mod(vec![
                    AstNode::Function(
                        FunctionHeader::new(Identifier("foo".to_string()), vec![], None),
                        ScopedBlock(vec![]),
                    ),
                ])
            )
        }

        #[test]
        fn fn_with_return() {
            assert_eq!(
                parse("fn foo() -> bar {}").unwrap(),
                AstNode::Mod(vec![
                    AstNode::Function(
                        FunctionHeader::new(
                            Identifier("foo".to_string()),
                            vec![],
                            Some(Identifier("bar".to_string())),
                        ),
                        ScopedBlock(vec![]),
                    ),
                ])
            )
        }
    }
}
