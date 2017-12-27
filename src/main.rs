mod lexer;
mod parser;
mod error;

mod ast {
    #[derive(Debug)]
    pub struct Identifier(pub String);
    #[derive(Debug)]
    pub struct ScopedBlock(pub Vec<AstNode>);
    #[derive(Debug)]
    pub struct FunctionHeader(
        Identifier,
        Vec<(Identifier, Identifier)>,
        Option<Identifier>,
    );

    #[derive(Debug)]
    pub struct If(pub Box<Expr>, pub ScopedBlock, pub Option<ScopedBlock>);

    #[derive(Debug)]
    pub enum Primary {
        If(If),
        Integer(u64),
        Identifier(Identifier),
    }

    #[derive(Debug)]
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

    #[derive(Debug)]
    pub enum AstNode {
        Token(::token::Token),
        Mod(Vec<AstNode>),
        Function(FunctionHeader, ScopedBlock),
        ScopedBlock(ScopedBlock),
        Expr(Expr),
    }
}

mod token {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Token {
        pub loc: super::util::Loc,
        pub token_type: TokenType,
    }

    impl Token {
        pub fn require(&self, expected: &TokenType) -> ::error::RefTokenResult {
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

mod util {
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

fn run_parser() -> Result<(), Box<std::error::Error>> {
    use lexer::Lexer;
    println!(
        "{:?}",
        parser::Parser::parse(&Lexer::new("fn a() {if 0 {}}").lex_all()?)?
    );
    Ok(())
}

fn main() {
    match run_parser() {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    }
}
