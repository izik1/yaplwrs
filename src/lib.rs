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

        fn run_test_inside_fn(code: &str, block: ScopedBlock) {
            assert_eq!(
                parse(&format!("fn foo() {{ {} }}", code)).unwrap(),
                foo_no_args(block)
            )
        }

        fn foo_no_args(body: ScopedBlock) -> AstNode {
            AstNode::Mod(vec![
                AstNode::Function(FunctionHeader::new(identifier("foo"), vec![], None), body),
            ])
        }

        fn prim_i32(val: i32) -> Expr {
            Expr::Primary(Primary::Integer(val.to_string(), "i32".to_string()))
        }

        fn identifier(id: &str) -> Identifier {
            Identifier(id.to_string())
        }

        #[test]
        fn empty() {
            assert_eq!(parse("").unwrap(), ast::AstNode::Mod(vec![]))
        }

        #[test]
        fn scope() {
            run_test_inside_fn(
                "{}",
                ScopedBlock(vec![AstNode::ScopedBlock(ScopedBlock(vec![]))]),
            )
        }

        #[test]
        fn bin_expressions() {
            run_test_inside_fn(
                "if a + b * 5 {10}",
                ScopedBlock(vec![
                    AstNode::Expr(Expr::Primary(Primary::If(If(
                        Box::new(Expr::Binary(
                            BinOperator::Plus,
                            Box::new(Expr::Primary(Primary::Identifier(identifier("a")))),
                            Box::new(Expr::Binary(
                                BinOperator::Star,
                                Box::new(Expr::Primary(Primary::Identifier(identifier("b")))),
                                Box::new(prim_i32(5)),
                            )),
                        )),
                        ScopedBlock(vec![AstNode::Expr(prim_i32(10))]),
                        vec![],
                        None,
                    )))),
                ]),
            )
        }

        #[test]
        fn if_elseif_else() {
            assert_eq!(
                parse("fn foo() { q + if a {1} else if b {2} else if c {3} else {4} }").unwrap(),
                foo_no_args(ScopedBlock(vec![
                    AstNode::Expr(Expr::Binary(
                        BinOperator::Plus,
                        Box::new(Expr::Primary(Primary::Identifier(identifier("q")))),
                        Box::new(Expr::Primary(Primary::If(If(
                            Box::new(Expr::Primary(Primary::Identifier(identifier("a")))),
                            ScopedBlock(vec![AstNode::Expr(prim_i32(1))]),
                            vec![
                                ElseIf(
                                    Box::new(Expr::Primary(Primary::Identifier(identifier("b")))),
                                    ScopedBlock(vec![AstNode::Expr(prim_i32(2))]),
                                ),
                                ElseIf(
                                    Box::new(Expr::Primary(Primary::Identifier(identifier("c")))),
                                    ScopedBlock(vec![AstNode::Expr(prim_i32(3))]),
                                ),
                            ],
                            Some(ScopedBlock(vec![AstNode::Expr(prim_i32(4))])),
                        )))),
                    )),
                ]))
            )
        }

        #[test]
        fn expressions_unary_minus() {
            run_test_inside_fn(
                "-a --b",
                ScopedBlock(vec![
                    AstNode::Expr(Expr::Binary(
                        BinOperator::Minus,
                        Box::new(Expr::Unary(
                            UnaryOperator::Minus,
                            Box::new(Expr::Primary(Primary::Identifier(identifier("a")))),
                        )),
                        Box::new(Expr::Unary(
                            UnaryOperator::Minus,
                            Box::new(Expr::Primary(Primary::Identifier(identifier("b")))),
                        )),
                    )),
                ]),
            )
        }

        mod function {
            use super::*;

            #[test]
            fn empty_fn() {
                run_test_inside_fn("", ScopedBlock(vec![]))
            }

            #[test]
            fn with_return() {
                assert_eq!(
                    parse("fn foo() -> bar {}").unwrap(),
                    AstNode::Mod(vec![
                        AstNode::Function(
                            FunctionHeader::new(identifier("foo"), vec![], Some(identifier("bar"))),
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
                                identifier("foo"),
                                vec![
                                    (identifier("q"), identifier("bar")),
                                    (identifier("z"), identifier("u32")),
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
                                identifier("foo"),
                                vec![
                                    (identifier("q"), identifier("bar")),
                                    (identifier("z"), identifier("u32")),
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
