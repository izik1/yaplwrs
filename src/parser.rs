use error::*;
use token::*;
use ast::*;

pub struct Parser<'a> {
    tokens: &'a Vec<Token>,
    pos: usize,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<Token>) -> Parser {
        Parser { tokens, pos: 0 }
    }

    pub fn parse(tokens: &'a Vec<Token>) -> AbstractSyntaxTreeResult {
        Parser::new(tokens).parse_mod()
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, CompilerError> {
        let token = self.peek()?.clone();
        match token.token_type {
            TokenType::Grammar(Grammar::Plus) => {
                Err(CompilerError::with_loc("Unary plus is invalid", token.loc))
            }
            _ => Ok(Expr::Primary(self.parse_primary()?)),
        }
    }

    fn parse_primary(&mut self) -> Result<Primary, CompilerError> {
        let token = self.advance()?.clone();
        match token.token_type {
            TokenType::Identifier(id) => Ok(Primary::Identifier(Identifier(id))),
            TokenType::Integer(i) => Ok(Primary::Integer(i)),
            TokenType::If => Ok(Primary::If(If(
                self.parse_expr()?,
                self.parse_scoped_block()?,
                None,
            ))),
            _ => Err(CompilerError::with_loc("Invalid primary", token.loc)),
        }
    }

    fn parse_expr(&mut self) -> Result<Box<Expr>, CompilerError> {
        let lhs = Box::new(self.parse_unary_expr()?);
        self.parse_bin_expr(lhs, 0)
    }

    fn parse_bin_expr(
        &mut self,
        lhs: Box<Expr>,
        min_priority: usize,
    ) -> Result<Box<Expr>, CompilerError> {
        fn is_bin_op(token: &TokenType) -> bool {
            match *token {
                TokenType::Grammar(ref g) => match *g {
                    Grammar::Plus | Grammar::Slash | Grammar::Minus | Grammar::Star => true,
                    _ => false,
                },
                _ => false,
            }
        }

        fn precedence(token: &TokenType) -> Result<usize, CompilerError> {
            match *token {
                TokenType::Grammar(ref g) => match *g {
                    Grammar::Plus | Grammar::Minus => Ok(150),
                    Grammar::Star | Grammar::Slash => Ok(200),
                    _ => Err(CompilerError::ice()),
                },

                _ => Err(CompilerError::ice()),
            }
        }

        let res = lhs;
        let lookahead = self.peek()?;
        while is_bin_op(&lookahead.token_type) && precedence(&lookahead.token_type)? > min_priority
        {
            unimplemented!()
        }

        Ok(res)
    }

    fn parse_scoped_block(&mut self) -> Result<ScopedBlock, CompilerError> {
        self.advance()?
            .require(&TokenType::Grammar(Grammar::OpenBrace))?;

        let mut vec: Vec<AstNode> = Vec::new();
        loop {
            let token = self.peek()?.clone();
            vec.push(match token.token_type {
                TokenType::Grammar(Grammar::OpenBrace) => {
                    Ok(AstNode::ScopedBlock(self.parse_scoped_block()?))
                }

                TokenType::Grammar(Grammar::CloseBrace) => {
                    self.advance()?;
                    break;
                }

                _ => Ok(AstNode::Expr(Expr::unbox(self.parse_expr()?))),
            }?);
        }

        Ok(ScopedBlock(vec))
    }

    fn parse_mod(&mut self) -> AbstractSyntaxTreeResult {
        let mut vec: Vec<AstNode> = Vec::new();
        while self.has_tokens() {
            let token = self.advance().unwrap().clone();
            vec.push(match token.token_type {
                TokenType::Function => {
                    let name = self.require_identifier()?;
                    self.advance()?
                        .require(&TokenType::Grammar(Grammar::OpenParen))?;
                    self.advance()?
                        .require(&TokenType::Grammar(Grammar::CloseParen))?;
                    Ok(AstNode::Function(
                        FunctionHeader::new(name, Vec::new(), None),
                        self.parse_scoped_block()?,
                    ))
                }

                _ => Err(CompilerError::with_loc(
                    "Parser: unexpected token for module",
                    token.loc,
                )),
            }?);
        }

        Ok(AstNode::Mod(vec))
    }

    fn has_tokens(&self) -> bool {
        self.pos < self.tokens.len()
    }

    fn peek(&self) -> RefTokenResult {
        if self.pos >= self.tokens.len() {
            Err(CompilerError::new("Parser: Unexpected end of file"))
        } else {
            Ok(&self.tokens[self.pos])
        }
    }

    fn require_identifier(&mut self) -> Result<Identifier, CompilerError> {
        let token = self.advance()?.clone();

        if let TokenType::Identifier(id) = token.token_type {
            Ok(Identifier(id))
        } else {
            Err(CompilerError::with_loc(
                "Parser: Expected Identifier, but got something else",
                token.loc,
            ))
        }
    }

    fn advance(&mut self) -> RefTokenResult {
        if self.pos >= self.tokens.len() {
            Err(CompilerError::new("Parser: Unexpected end of file"))
        } else {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Ok(tok)
        }
    }
}
