#![feature(nll, box_syntax)]

#[macro_use]
extern crate maplit;

#[cfg(test)]
#[macro_use]
extern crate proptest;

mod ast;
pub mod error;
mod lexer;
mod parser;
pub mod token;
pub mod util;

pub fn lex(string: &str) -> Result<Vec<token::Token>, error::Error> {
    Ok(lexer::Lexer::new(string)?.lex_all())
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
            AstNode::Mod(box [AstNode::Function(Function::new(
                FunctionHeader::new(ident("foo"), box [], None),
                body,
            ))])
        }

        fn prim_int(val: i32) -> Expr {
            Expr::Primary(Primary::Integer(val.to_string(), None))
        }

        fn ident(id: &str) -> Ident {
            Ident(id.to_string())
        }

        #[test]
        fn empty() {
            assert_eq!(parse("").unwrap(), ast::AstNode::Mod(box []))
        }

        #[test]
        fn fn_call() {
            run_test_inside_fn(
                "foo(a, b)",
                ScopedBlock(box [AstNode::Expr(Expr::Primary(Primary::FunctionCall(
                    ident("foo"),
                    box [
                        Expr::Primary(Primary::Ident(ident("a"))),
                        Expr::Primary(Primary::Ident(ident("b"))),
                    ],
                )))]),
            )
        }

        #[test]
        fn scope() {
            run_test_inside_fn(
                "{}",
                ScopedBlock(box [AstNode::ScopedBlock(ScopedBlock(box []))]),
            )
        }

        #[test]
        fn if_elseif_else() {
            assert_eq!(
                parse("fn foo() { q + if a {1} else if b {2} else if c {3} else {4} }").unwrap(),
                foo_no_args(ScopedBlock(box [AstNode::Expr(Expr::Binary(
                    BinOperator::BinAdd,
                    box Expr::Primary(Primary::Ident(ident("q"))),
                    box Expr::Primary(Primary::If(If(
                        box (Expr::Primary(Primary::Ident(ident("a")))),
                        ScopedBlock(box [AstNode::Expr(prim_int(1))]),
                        box [
                            ElseIf(
                                box Expr::Primary(Primary::Ident(ident("b"))),
                                ScopedBlock(box [AstNode::Expr(prim_int(2))]),
                            ),
                            ElseIf(
                                box Expr::Primary(Primary::Ident(ident("c"))),
                                ScopedBlock(box [AstNode::Expr(prim_int(3))]),
                            ),
                        ],
                        Some(ScopedBlock(box [AstNode::Expr(prim_int(4))])),
                    ))),
                ))]))
            )
        }

        mod expression {
            use super::*;

            #[test]
            fn binary() {
                run_test_inside_fn(
                    "10 + 4 * 5 / 2 - 1",
                    ScopedBlock(box [AstNode::Expr(Expr::Binary(
                        BinOperator::BinSub,
                        box Expr::Binary(
                            BinOperator::BinAdd,
                            box prim_int(10),
                            box Expr::Binary(
                                BinOperator::BinDiv,
                                box Expr::Binary(
                                    BinOperator::BinMul,
                                    box prim_int(4),
                                    box prim_int(5),
                                ),
                                box prim_int(2),
                            ),
                        ),
                        box prim_int(1),
                    ))]),
                )
            }

            #[test]
            fn unary_minus() {
                run_test_inside_fn(
                    "-a --b",
                    ScopedBlock(box [AstNode::Expr(Expr::Binary(
                        BinOperator::BinSub,
                        box Expr::Unary(
                            UnaryOperator::UnNeg,
                            box Expr::Primary(Primary::Ident(ident("a"))),
                        ),
                        box Expr::Unary(
                            UnaryOperator::UnNeg,
                            box Expr::Primary(Primary::Ident(ident("b"))),
                        ),
                    ))]),
                )
            }

        }

        mod function {
            use super::*;

            proptest! {
                #[test]
                fn empty_fn(ref id in "[A-Za-z_][A-Za-z0-9]*") {
                prop_assume!(!lexer::is_keyword(id));
                println!("fn {}() -> {{}}", id);
                     prop_assert_eq!(
                        parse(&format!("fn {}() {{}}", id)).unwrap(),
                        AstNode::Mod(box [
                            AstNode::Function(Function::new(
                                FunctionHeader::new(ident(id), box [], None),
                                ScopedBlock(box []),
                            ))
                        ])
                    )
                }
            }

            #[test]
            #[should_panic]
            fn empty_fn_keyword_as_name() {
                parse("fn if() {}").unwrap();
            }

            #[test]
            fn with_return() {
                assert_eq!(
                    parse("fn foo() -> bar {}").unwrap(),
                    AstNode::Mod(box [AstNode::Function(Function::new(
                        FunctionHeader::new(ident("foo"), box [], Some(ident("bar"))),
                        ScopedBlock(box []),
                    ))])
                )
            }

            #[test]
            fn with_args() {
                assert_eq!(
                    parse("fn foo(q: bar, z: u32) {}").unwrap(),
                    AstNode::Mod(box [AstNode::Function(Function::new(
                        FunctionHeader::new(
                            ident("foo"),
                            box [(ident("q"), ident("bar")), (ident("z"), ident("u32"))],
                            None,
                        ),
                        ScopedBlock(box []),
                    ))])
                )
            }

            #[test]
            #[should_panic]
            fn args_without_separation() {
                parse("fn foo(q: bar z: u32) {}").unwrap();
            }
        }
    }
}
