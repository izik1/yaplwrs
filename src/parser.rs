use error::*;
use token::*;
use ast::*;

struct TokenIterator<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> TokenIterator<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        TokenIterator { tokens, pos: 0 }
    }

    pub fn peek(&self) -> Result<&Token> {
        if self.pos < self.tokens.len() {
            Ok(&self.tokens[self.pos])
        } else {
            Err(Error::Other("Parser: Unexpected end of tokens"))
        }
    }

    pub fn move_required(&mut self, token_type: &TokenType) -> Result<()> {
        self.next()?.require(token_type)?;
        Ok(())
    }

    pub fn next(&mut self) -> Result<&Token> {
        if self.pos < self.tokens.len() {
            let tmp = Ok(&self.tokens[self.pos]);
            self.pos += 1;
            tmp
        } else {
            Err(Error::Other("Parser: Unexpected end of tokens"))
        }
    }

    pub fn move_next(&mut self) -> Result<()> {
        if self.pos < self.tokens.len() {
            self.pos += 1;
            Ok(())
        } else {
            Err(Error::Other("Parser: Unexpected end of tokens"))
        }
    }

    pub fn empty(&self) -> bool {
        self.pos >= self.tokens.len()
    }
}

pub fn parse(tokens: &[Token]) -> Result<AstNode> {
    parse_mod(TokenIterator::new(tokens))
}

fn parse_unary_expr(tokens: &mut TokenIterator) -> Result<Expr> {
    let token = tokens.peek()?;

    if let Some(op) = UnaryOperator::from_token_type(&token.token_type) {
        tokens.move_next()?;
        return Ok(Expr::Unary(op, Box::new(parse_unary_expr(tokens)?)));
    }

    match token.token_type {
        TokenType::Grammar(Grammar::Plus) => Err(Error::Span(SpanError::new(
            "Parser: Unary plus is invalid".to_string(),
            token.span,
        ))),

        _ => Ok(Expr::Primary(parse_primary(tokens)?)),
    }
}

// NOTE: this assumes that the caller already ate the "if" token, and doesn't check for it.
fn parse_if(tokens: &mut TokenIterator) -> Result<If> {
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

fn parse_var_with_type(tokens: &mut TokenIterator) -> Result<(Identifier, Identifier)> {
    let name = next_identifier(tokens)?;
    tokens.move_required(&TokenType::Grammar(Grammar::Colon))?;
    Ok((name, next_identifier(tokens)?))
}

fn parse_call(tokens: &mut TokenIterator, id: Identifier) -> Result<Primary> {
    tokens.move_required(&TokenType::Grammar(Grammar::OpenParen))?;
    let mut args = vec![];
    loop {
        if tokens.peek()?.token_type == TokenType::Grammar(Grammar::CloseParen) {
            tokens.move_next().unwrap();
            break;
        } else {
            args.push(*parse_expr(tokens)?);
            if tokens.peek()?.token_type == TokenType::Grammar(Grammar::Comma) {
                tokens.move_next().unwrap();
            }
        }
    }

    Ok(Primary::FunctionCall(id, args))
}

fn parse_primary(tokens: &mut TokenIterator) -> Result<Primary> {
    let token = tokens.next()?;
    match token.token_type {
        TokenType::Identifier(ref id) => {
            let id = Identifier(id.clone());
            if tokens.peek()?.token_type == TokenType::Grammar(Grammar::OpenParen) {
                parse_call(tokens, id)
            } else {
                Ok(Primary::Identifier(id))
            }
        }

        TokenType::Integer(ref i, ref suffix) => Ok(Primary::Integer(i.clone(), suffix.clone())),
        TokenType::Keyword(Keyword::If) => Ok(Primary::If(parse_if(tokens)?)),
        _ => Err(Error::Span(SpanError::new(
            "Parser: Invalid primary".to_string(),
            token.span,
        ))),
    }
}

fn parse_expr(tokens: &mut TokenIterator) -> Result<Box<Expr>> {
    let lhs = Box::new(parse_unary_expr(tokens)?);
    parse_bin_expr(tokens, lhs, 0)
}

fn parse_bin_expr(
    tokens: &mut TokenIterator,
    mut lhs: Box<Expr>,
    min_priority: usize,
) -> Result<Box<Expr>> {
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

fn parse_scoped_block(tokens: &mut TokenIterator) -> Result<ScopedBlock> {
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

            _ => vec.push(AstNode::Expr(*parse_expr(tokens)?)),
        };
    }

    Ok(ScopedBlock(vec))
}

fn next_identifier(tokens: &mut TokenIterator) -> Result<Identifier> {
    let tok = tokens.next()?;
    match tok.token_type {
        TokenType::Identifier(ref s) => Ok(Identifier(s.clone())),
        _ => Err(Error::Span(SpanError::new(
            "Parser: Expected Identifier but got something else".to_string(),
            tok.span,
        ))),
    }
}

fn parse_fn(tokens: &mut TokenIterator) -> Result<AstNode> {
    tokens.move_required(&TokenType::Keyword(Keyword::Function))?;
    let name = next_identifier(tokens)?;
    tokens.move_required(&TokenType::Grammar(Grammar::OpenParen))?;
    let mut args: Vec<(Identifier, Identifier)> = Vec::new();

    if let TokenType::Identifier(_) = tokens.peek()?.token_type {
        loop {
            args.push(parse_var_with_type(tokens)?);
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

    Ok(AstNode::Function(Function::new(
        FunctionHeader::new(name, args, return_type),
        parse_scoped_block(tokens)?,
    )))
}

fn parse_mod(mut tokens: TokenIterator) -> Result<AstNode> {
    let mut vec: Vec<AstNode> = Vec::new();
    while !tokens.empty() {
        let token = tokens.peek().unwrap();
        match token.token_type {
            TokenType::Keyword(Keyword::Function) => vec.push(parse_fn(&mut tokens)?),
            _ => {
                return Err(Error::Span(SpanError::new(
                    "Parser: unexpected token for module".to_string(),
                    token.span,
                )))
            }
        };
    }

    Ok(AstNode::Mod(vec))
}
