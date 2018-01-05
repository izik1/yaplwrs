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

    pub fn peek(&self) -> CompilerResult<&Token> {
        if self.pos < self.tokens.len() {
            Ok(&self.tokens[self.pos])
        } else {
            Err(CompilerError::new("Parser: Unexpected end of tokens"))
        }
    }

    pub fn move_required(&mut self, token_type: &TokenType) -> CompilerResult<()> {
        self.next()?.require(token_type)?;
        Ok(())
    }

    pub fn next(&mut self) -> CompilerResult<&Token> {
        if self.pos < self.tokens.len() {
            let tmp = Ok(&self.tokens[self.pos]);
            self.pos += 1;
            tmp
        } else {
            Err(CompilerError::new("Parser: Unexpected end of tokens"))
        }
    }

    pub fn move_next(&mut self) -> CompilerResult<()> {
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

pub fn parse<'a>(tokens: &'a Vec<Token>) -> CompilerResult<AstNode> {
    parse_mod(TokenIterator::new(tokens))
}

fn parse_unary_expr(tokens: &mut TokenIterator) -> CompilerResult<Expr> {
    let token = tokens.peek()?;

    if let Some(op) = UnaryOperator::from_token_type(&token.token_type) {
        tokens.move_next()?;
        return Ok(Expr::Unary(op, Box::new(parse_unary_expr(tokens)?)));
    }

    match token.token_type {
        TokenType::Grammar(Grammar::Plus) => Err(CompilerError::with_loc(
            "Parser: Unary plus is invalid",
            token.loc,
        )),

        _ => Ok(Expr::Primary(parse_primary(tokens)?)),
    }
}

// NOTE: this assumes that the caller already ate the "if" token, and doesn't check for it.
fn parse_if(tokens: &mut TokenIterator) -> CompilerResult<If> {
    let cond = parse_expr(tokens)?;
    let block = parse_scoped_block(tokens)?;
    let mut elseifs = vec![];
    let mut block_else = None;
    loop {
        if tokens.peek()?.token_type == TokenType::Keyword(Keyword::Else) {
            tokens.move_next().unwrap();
            if tokens.peek()?.token_type == TokenType::Keyword(Keyword::If) {
                tokens.move_next().unwrap();
                elseifs.push(ElseIf(parse_expr(tokens)?, parse_scoped_block(tokens)?));
            } else {
                block_else = Some(parse_scoped_block(tokens)?);
                break;
            }
        } else {
            break;
        }
    }

    Ok(If(cond, block, elseifs, block_else))
}

fn parse_primary(tokens: &mut TokenIterator) -> CompilerResult<Primary> {
    let token = tokens.next()?;
    match token.token_type {
        TokenType::Identifier(ref id) => Ok(Primary::Identifier(Identifier(id.clone()))),
        TokenType::Integer(ref i, ref suffix) => Ok(Primary::Integer(i.clone(), suffix.clone())),
        TokenType::Keyword(Keyword::If) => Ok(Primary::If(parse_if(tokens)?)),
        _ => Err(CompilerError::with_loc(
            "Parser: Invalid primary",
            token.loc,
        )),
    }
}

fn parse_expr(tokens: &mut TokenIterator) -> CompilerResult<Box<Expr>> {
    let lhs = Box::new(parse_unary_expr(tokens)?);
    parse_bin_expr(tokens, lhs, 0)
}

fn parse_bin_expr(
    tokens: &mut TokenIterator,
    mut lhs: Box<Expr>,
    min_priority: usize,
) -> CompilerResult<Box<Expr>> {
    fn precedence_compare(a: &BinOperator, b: &BinOperator) -> bool {
        a.precedence() > b.precedence()
            || (a.precedence() == b.precedence() && (a.associativity() == Associativity::Right))
    }

    let mut lookahead = tokens.peek()?;
    while let Some(op) = BinOperator::from_token_type(&lookahead.token_type) {
        if op.precedence() < min_priority {
            break;
        }

        tokens.move_next().unwrap();

        let mut rhs = Box::new(parse_unary_expr(tokens)?);
        lookahead = tokens.peek()?;

        while let Some(lookahead_op) = BinOperator::from_token_type(&lookahead.token_type) {
            if !precedence_compare(&lookahead_op, &op) {
                break;
            }

            rhs = parse_bin_expr(tokens, rhs, lookahead_op.precedence())?;
            lookahead = tokens.peek()?;
        }

        lhs = Box::new(Expr::Binary(op, lhs, rhs));
    }

    Ok(lhs)
}

fn parse_scoped_block(tokens: &mut TokenIterator) -> CompilerResult<ScopedBlock> {
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

fn next_identifier(tokens: &mut TokenIterator) -> CompilerResult<Identifier> {
    let tok = tokens.next()?;
    match tok.token_type {
        TokenType::Identifier(ref s) => Ok(Identifier(s.clone())),
        _ => Err(CompilerError::with_loc(
            "Parser: Expected Identifier but got something else",
            tok.loc,
        )),
    }
}

fn parse_fn(tokens: &mut TokenIterator) -> CompilerResult<AstNode> {
    tokens.move_required(&TokenType::Keyword(Keyword::Function))?;
    let name = next_identifier(tokens)?;
    tokens.move_required(&TokenType::Grammar(Grammar::OpenParen))?;
    let mut args: Vec<(Identifier, Identifier)> = Vec::new();

    if let TokenType::Identifier(_) = tokens.peek()?.token_type {
        loop {
            let name = next_identifier(tokens)?;
            tokens.move_required(&TokenType::Grammar(Grammar::Colon))?;
            args.push((name, next_identifier(tokens)?));
            if tokens.peek()?.token_type == TokenType::Grammar(Grammar::Comma) {
                tokens.move_next().unwrap();
            } else {
                break;
            }
        }
    }

    tokens.move_required(&TokenType::Grammar(Grammar::CloseParen))?;

    let return_type = match tokens.peek()?.token_type {
        TokenType::Grammar(Grammar::Arrow) => {
            tokens.move_next().unwrap();
            Some(next_identifier(tokens)?)
        }

        _ => None,
    };

    Ok(AstNode::Function(
        FunctionHeader::new(name, args, return_type),
        parse_scoped_block(tokens)?,
    ))
}

fn parse_mod(mut tokens: TokenIterator) -> CompilerResult<AstNode> {
    let mut vec: Vec<AstNode> = Vec::new();
    while !tokens.empty() {
        let token = tokens.peek().unwrap();
        match token.token_type {
            TokenType::Keyword(Keyword::Function) => vec.push(parse_fn(&mut tokens)?),
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
