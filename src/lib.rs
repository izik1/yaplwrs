#![feature(nll)]

#[macro_use]
extern crate maplit;

pub mod error;
pub mod token;
pub mod util;
mod ast;
mod lexer;
mod parser;

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
        fn expressions() {
            assert_eq!(
                parse("fn foo() { if a + b * 5 {10} }").unwrap(),
                AstNode::Mod(vec![
                    AstNode::Function(
                        FunctionHeader::new(Identifier("foo".to_string()), vec![], None),
                        ScopedBlock(vec![
                            AstNode::Expr(Expr::Primary(Primary::If(If(
                                Box::new(Expr::Binary(
                                    BinOperator::Plus,
                                    Box::new(Expr::Primary(Primary::Identifier(Identifier(
                                        "a".to_string(),
                                    )))),
                                    Box::new(Expr::Binary(
                                        BinOperator::Star,
                                        Box::new(Expr::Primary(Primary::Identifier(Identifier(
                                            "b".to_string(),
                                        )))),
                                        Box::new(Expr::Primary(Primary::Integer(
                                            "5".to_string(),
                                            "i32".to_string(),
                                        ))),
                                    )),
                                )),
                                ScopedBlock(vec![
                                    AstNode::Expr(Expr::Primary(Primary::Integer(
                                        "10".to_string(),
                                        "i32".to_string(),
                                    ))),
                                ]),
                                None,
                            )))),
                        ]),
                    ),
                ])
            )
        }

        mod function {
            use super::*;

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
            fn with_return() {
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

            #[test]
            fn with_args() {
                assert_eq!(
                    parse("fn foo(q: bar, z: u32) {}").unwrap(),
                    AstNode::Mod(vec![
                        AstNode::Function(
                            FunctionHeader::new(
                                Identifier("foo".to_string()),
                                vec![
                                    (Identifier("q".to_string()), Identifier("bar".to_string())),
                                    (Identifier("z".to_string()), Identifier("u32".to_string())),
                                ],
                                None,
                            ),
                            ScopedBlock(vec![]),
                        ),
                    ])
                )
            }

            #[test]
            #[should_panic]
            fn args_without_separation() {
                assert_ne!(
                    parse("fn foo(q: bar z: u32) {}").unwrap(),
                    AstNode::Mod(vec![
                        AstNode::Function(
                            FunctionHeader::new(
                                Identifier("foo".to_string()),
                                vec![
                                    (Identifier("q".to_string()), Identifier("bar".to_string())),
                                    (Identifier("z".to_string()), Identifier("u32".to_string())),
                                ],
                                None,
                            ),
                            ScopedBlock(vec![]),
                        ),
                    ])
                )
            }
        }
    }
}
