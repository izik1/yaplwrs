use error::*;
use token::*;
use ast::*;

struct TokenIterator<'a> {
    tokens: &'a Vec<Token>,
    pos: usize,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a Vec<Token>) -> Self {
        TokenIterator { tokens, pos: 0 }
    }

    pub fn peek(&self) -> Result<&Token, CompilerError> {
        if self.pos < self.tokens.len() {
            Ok(&self.tokens[self.pos])
        } else {
            Err(CompilerError::new("Parser: Unexpected end of tokens"))
        }
    }

    pub fn move_required(&mut self, token_type: &TokenType) -> Result<(), CompilerError> {
        self.peek()?.require(token_type)?;
        self.move_next().unwrap(); // If peek is successful, moving should be as well.
        Ok(())
    }

    pub fn move_next(&mut self) -> Result<(), CompilerError> {
        if self.pos < self.tokens.len() {
            self.pos += 1;
            Ok(())
        } else {
            Err(CompilerError::new("Parser: Unexpected end of tokens"))
        }
    }

    pub fn empty(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}

pub fn parse<'a>(tokens: &'a Vec<Token>) -> Result<AstNode, CompilerError> {
    parse_mod(TokenIterator::new(tokens))
}

fn parse_unary_expr(tokens: &mut TokenIterator) -> Result<Expr, CompilerError> {
    let token = tokens.peek()?.clone();
    match token.token_type {
        TokenType::Grammar(Grammar::Plus) => Err(CompilerError::with_loc(
            "Parser: Unary plus is invalid",
            token.loc,
        )),

        _ => Ok(Expr::Primary(parse_primary(tokens)?)),
    }
}

fn parse_primary(tokens: &mut TokenIterator) -> Result<Primary, CompilerError> {
    let token = tokens.peek()?.clone();
    tokens.move_next().unwrap();
    match token.token_type {
        TokenType::Identifier(id) => Ok(Primary::Identifier(Identifier(id))),
        TokenType::Integer(i) => Ok(Primary::Integer(i)),
        TokenType::If => Ok(Primary::If(If(
            parse_expr(tokens)?,
            parse_scoped_block(tokens)?,
            None,
        ))),
        _ => Err(CompilerError::with_loc(
            "Parser: Invalid primary",
            token.loc,
        )),
    }
}

fn parse_expr(tokens: &mut TokenIterator) -> Result<Box<Expr>, CompilerError> {
    let lhs = Box::new(parse_unary_expr(tokens)?);
    parse_bin_expr(tokens, lhs, 0)
}

fn parse_bin_expr(
    tokens: &mut TokenIterator,
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
    let lookahead = tokens.peek()?;
    while is_bin_op(&lookahead.token_type) && precedence(&lookahead.token_type)? > min_priority {
        unimplemented!()
    }

    Ok(res)
}

fn parse_scoped_block(tokens: &mut TokenIterator) -> Result<ScopedBlock, CompilerError> {
    tokens.move_required(&TokenType::Grammar(Grammar::OpenBrace))?;

    let mut vec: Vec<AstNode> = Vec::new();
    loop {
        match tokens.peek()?.token_type {
            TokenType::Grammar(Grammar::OpenBrace) => {
                vec.push(AstNode::ScopedBlock(parse_scoped_block(tokens)?))
            }

            TokenType::Grammar(Grammar::CloseBrace) => {
                tokens.move_next().unwrap();
                break;
            }

            _ => vec.push(AstNode::Expr(Expr::unbox(parse_expr(tokens)?))),
        };
    }

    Ok(ScopedBlock(vec))
}

fn parse_mod(mut tokens: TokenIterator) -> Result<AstNode, CompilerError> {
    let mut vec: Vec<AstNode> = Vec::new();
    while !tokens.empty() {
        let token = tokens.peek().unwrap().clone();
        tokens.move_next().unwrap();
        match token.token_type {
            TokenType::Function => {
                let Token { token_type, loc } = tokens.peek()?.clone();
                if let TokenType::Identifier(name) = token_type {
                    tokens.move_next().unwrap();
                    tokens.move_required(&TokenType::Grammar(Grammar::OpenParen))?;
                    tokens.move_required(&TokenType::Grammar(Grammar::CloseParen))?;

                    vec.push(AstNode::Function(
                        FunctionHeader::new(Identifier(name), Vec::new(), None),
                        parse_scoped_block(&mut tokens)?,
                    ));
                } else {
                    return Err(CompilerError::with_loc(
                        "Parser: Expected Identifier but got something else",
                        loc,
                    ));
                }
            }

            _ => {
                return Err(CompilerError::with_loc(
                    "Parser: unexpected token for module",
                    token.loc,
                ))
            }
        };
    }

    Ok(AstNode::Mod(vec))
}
