#![feature(nll, catch_expr)]

#[macro_use]
extern crate maplit;

#[cfg(test)]
#[macro_use]
extern crate proptest;

pub mod error;
pub mod token;
pub mod util;
mod ast;
mod lexer;
mod parser;

pub fn lex(string: &str) -> Result<Vec<token::Token>, error::Error> {
    Ok(lexer::Lexer::new(string)?.lex_all()?)
}

pub fn parse(string: &str) -> Result<ast::AstNode, error::Error> {
    Ok(parser::parse(&lex(string)?)?)
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
                AstNode::Function(Function::new(
                    FunctionHeader::new(identifier("foo"), vec![], None),
                    body,
                )),
            ])
        }

        fn prim_int(val: i32) -> Expr {
            Expr::Primary(Primary::Integer(val.to_string(), None))
        }

        fn identifier(id: &str) -> Identifier {
            Identifier(id.to_string())
        }

        #[test]
        fn empty() {
            assert_eq!(parse("").unwrap(), ast::AstNode::Mod(vec![]))
        }

        #[test]
        fn fn_call() {
            run_test_inside_fn(
                "foo(a, b)",
                ScopedBlock(vec![
                    AstNode::Expr(Expr::Primary(Primary::FunctionCall(
                        identifier("foo"),
                        vec![
                            Expr::Primary(Primary::Identifier(identifier("a"))),
                            Expr::Primary(Primary::Identifier(identifier("b"))),
                        ],
                    ))),
                ]),
            )
        }

        #[test]
        fn scope() {
            run_test_inside_fn(
                "{}",
                ScopedBlock(vec![AstNode::ScopedBlock(ScopedBlock(vec![]))]),
            )
        }

        #[test]
        fn if_elseif_else() {
            assert_eq!(
                parse("fn foo() { q + if a {1} else if b {2} else if c {3} else {4} }").unwrap(),
                foo_no_args(ScopedBlock(vec![
                    AstNode::Expr(Expr::Binary(
                        BinOperator::BinAdd,
                        Box::new(Expr::Primary(Primary::Identifier(identifier("q")))),
                        Box::new(Expr::Primary(Primary::If(If(
                            Box::new(Expr::Primary(Primary::Identifier(identifier("a")))),
                            ScopedBlock(vec![AstNode::Expr(prim_int(1))]),
                            vec![
                                ElseIf(
                                    Box::new(Expr::Primary(Primary::Identifier(identifier("b")))),
                                    ScopedBlock(vec![AstNode::Expr(prim_int(2))]),
                                ),
                                ElseIf(
                                    Box::new(Expr::Primary(Primary::Identifier(identifier("c")))),
                                    ScopedBlock(vec![AstNode::Expr(prim_int(3))]),
                                ),
                            ],
                            Some(ScopedBlock(vec![AstNode::Expr(prim_int(4))])),
                        )))),
                    )),
                ]))
            )
        }

        mod expression {
            use super::*;

            #[test]
            fn binary() {
                run_test_inside_fn(
                    "10 + 4 * 5 / 2 - 1",
                    ScopedBlock(vec![
                        AstNode::Expr(Expr::Binary(
                            BinOperator::BinSub,
                            Box::new(Expr::Binary(
                                BinOperator::BinAdd,
                                Box::new(prim_int(10)),
                                Box::new(Expr::Binary(
                                    BinOperator::BinDiv,
                                    Box::new(Expr::Binary(
                                        BinOperator::BinMul,
                                        Box::new(prim_int(4)),
                                        Box::new(prim_int(5)),
                                    )),
                                    Box::new(prim_int(2)),
                                )),
                            )),
                            Box::new(prim_int(1)),
                        )),
                    ]),
                )
            }

            #[test]
            fn unary_minus() {
                run_test_inside_fn(
                    "-a --b",
                    ScopedBlock(vec![
                        AstNode::Expr(Expr::Binary(
                            BinOperator::BinSub,
                            Box::new(Expr::Unary(
                                UnaryOperator::UnNeg,
                                Box::new(Expr::Primary(Primary::Identifier(identifier("a")))),
                            )),
                            Box::new(Expr::Unary(
                                UnaryOperator::UnNeg,
                                Box::new(Expr::Primary(Primary::Identifier(identifier("b")))),
                            )),
                        )),
                    ]),
                )
            }

        }

        mod function {
            use super::*;

            proptest! {
                #[test]
                fn empty_fn(ref id in "[A-Za-z_][A-Za-z0-9]*") {
                println!("fn {}() -> {{}}", id);
                     prop_assert_eq!(
                        parse(&format!("fn {}() {{}}", id)).unwrap(),
                        AstNode::Mod(vec![
                            AstNode::Function(Function::new(
                                FunctionHeader::new(identifier(id), vec![], None),
                                ScopedBlock(vec![]),
                            ))
                        ])
                    )
                }
            }

            #[test]
            fn with_return() {
                assert_eq!(
                    parse("fn foo() -> bar {}").unwrap(),
                    AstNode::Mod(vec![
                        AstNode::Function(Function::new(
                            FunctionHeader::new(identifier("foo"), vec![], Some(identifier("bar"))),
                            ScopedBlock(vec![]),
                        )),
                    ])
                )
            }

            #[test]
            fn with_args() {
                assert_eq!(
                    parse("fn foo(q: bar, z: u32) {}").unwrap(),
                    AstNode::Mod(vec![
                        AstNode::Function(Function::new(
                            FunctionHeader::new(
                                identifier("foo"),
                                vec![
                                    (identifier("q"), identifier("bar")),
                                    (identifier("z"), identifier("u32")),
                                ],
                                None,
                            ),
                            ScopedBlock(vec![]),
                        )),
                    ])
                )
            }

            #[test]
            #[should_panic]
            fn args_without_separation() {
                assert_ne!(
                    parse("fn foo(q: bar z: u32) {}").unwrap(),
                    AstNode::Mod(vec![
                        AstNode::Function(Function::new(
                            FunctionHeader::new(
                                identifier("foo"),
                                vec![
                                    (identifier("q"), identifier("bar")),
                                    (identifier("z"), identifier("u32")),
                                ],
                                None,
                            ),
                            ScopedBlock(vec![]),
                        )),
                    ])
                )
            }
        }
    }
}
