#![feature(box_syntax)]

#[macro_use]
extern crate maplit;

#[cfg(test)]
#[macro_use]
extern crate proptest;

use itertools::Itertools;

mod ast;
pub mod error;
mod lexer;
mod parser;
pub mod token;
pub mod util;

pub fn lex(string: &str) -> Result<Vec<token::Token>, error::Error> {
    Ok(lexer::Lexer::new(string)?.collect_vec())
}

pub fn parse(string: &str) -> Result<ast::AstNode, error::Error> {
    Ok(parser::parse(lexer::Lexer::new(string)?)?)
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parse {
        use super::*;
        use crate::ast::*;

        fn prim_int(val: i32) -> Expr {
            Expr::Primary(Primary::Integer(val.to_string(), None))
        }

        fn ident(id: &str) -> Ident {
            Ident(id.to_string())
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
            #[should_panic]
            fn args_without_separation() {
                parse("fn foo(q: bar z: u32) {}").unwrap();
            }
        }
    }
}
