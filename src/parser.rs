use crate::ast::*;
use crate::token::*;
use crate::util::Span;
use itertools::PeekingNext;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum Error {
    OutOfTokens,
    ReservedToken(String, Span),
    IncorrectToken {
        span: Span,
        expected: TokenType,
        actual: TokenType,
    },
    InvalidPrimaryExpression(Span),
    UnexpectedToken(Span, TokenType),
}

type Result<T> = ::std::result::Result<T, Error>;

struct TokenIterator<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> TokenIterator<T> {
    pub fn new(tokens: T) -> Self {
        TokenIterator {
            tokens: tokens.peekable(),
        }
    }

    pub fn peek(&mut self) -> Result<&Token> {
        self.tokens.peek().ok_or(Error::OutOfTokens)
    }

    fn peeking_next<F>(&mut self, accept: F) -> Option<Token>
    where
        F: FnOnce(&Token) -> bool,
    {
        self.tokens.peeking_next(accept)
    }

    pub fn move_required(&mut self, token_type: &TokenType) -> Result<()> {
        let tok = self.next()?;
        if tok.matches(token_type) {
            Ok(())
        } else {
            Err(Error::IncorrectToken {
                span: tok.span,
                expected: token_type.clone(),
                actual: tok.token_type.clone(),
            })
        }
    }

    pub fn next(&mut self) -> Result<Token> {
        self.tokens.next().ok_or(Error::OutOfTokens)
    }
}

pub fn parse<T: Iterator<Item = Token>>(tokens: T) -> Result<AstNode> {
    parse_mod(TokenIterator::new(tokens))
}

fn parse_unary_expr<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<Expr> {
    let token = tokens.peek()?;

    if let Some(op) = UnaryOperator::from_token_type(&token.token_type) {
        tokens.next()?;
        return Ok(Expr::Unary(op, box parse_unary_expr(tokens)?));
    }

    match token.token_type {
        TokenType::Grammar(Grammar::Plus) => {
            Err(Error::ReservedToken("Unary Plus".to_string(), token.span))
        }
        _ => Ok(Expr::Primary(parse_primary(tokens)?)),
    }
}

// NOTE: this assumes that the caller already ate the "if" token, and doesn't check for it.
fn parse_if<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<If> {
    let cond = parse_expr(tokens)?;
    let block = parse_scoped_block(tokens)?;
    let mut elseifs = vec![];
    let mut block_else = None;
    loop {
        if tokens.peek()?.token_type == TokenType::Keyword(Keyword::Else) {
            tokens.next().unwrap();
            if tokens.peek()?.token_type == TokenType::Keyword(Keyword::If) {
                tokens.next().unwrap();
                elseifs.push(ElseIf(box parse_expr(tokens)?, parse_scoped_block(tokens)?));
            } else {
                block_else = Some(parse_scoped_block(tokens)?);
                break;
            }
        } else {
            break;
        }
    }

    Ok(If(box cond, block, elseifs.into_boxed_slice(), block_else))
}

fn parse_var_with_type<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
) -> Result<(Ident, Ident)> {
    let name = next_ident(tokens)?;
    tokens.move_required(&TokenType::Grammar(Grammar::Colon))?;
    Ok((name, next_ident(tokens)?))
}

fn parse_call<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
    id: Ident,
) -> Result<Primary> {
    tokens.move_required(&TokenType::Grammar(Grammar::OpenParen))?;
    let mut args = vec![];
    loop {
        if tokens
            .peeking_next(|tok| tok.token_type == TokenType::Grammar(Grammar::CloseParen))
            .is_some()
        {
            break;
        } else {
            args.push(parse_expr(tokens)?);
            tokens.peeking_next(|tok| tok.token_type == TokenType::Grammar(Grammar::Comma));
        }
    }

    Ok(Primary::FunctionCall(id, args.into_boxed_slice()))
}

fn parse_primary<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<Primary> {
    let token = tokens.next()?;
    match token.token_type {
        TokenType::Ident(ref id) => {
            let id = Ident(id.clone());
            if tokens.peek()?.token_type == TokenType::Grammar(Grammar::OpenParen) {
                parse_call(tokens, id)
            } else {
                Ok(Primary::Ident(id))
            }
        }

        TokenType::Integer(ref i, ref suffix) => Ok(Primary::Integer(i.clone(), suffix.clone())),
        TokenType::Keyword(Keyword::If) => Ok(Primary::If(parse_if(tokens)?)),
        _ => Err(Error::InvalidPrimaryExpression(token.span)),
    }
}

fn parse_expr<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<Expr> {
    let lhs = parse_unary_expr(tokens)?;
    parse_bin_expr(tokens, lhs, 0)
}

fn parse_bin_expr<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
    mut lhs: Expr,
    min_priority: usize,
) -> Result<Expr> {
    fn precedence_compare(a: BinOperator, b: BinOperator) -> bool {
        a.precedence() > b.precedence()
            || (a.precedence() == b.precedence() && (a.associativity() == Associativity::Right))
    }

    let mut lookahead = tokens.peek()?;
    while let Some(op) = BinOperator::from_token_type(&lookahead.token_type) {
        if op.precedence() < min_priority {
            break;
        }

        tokens.next().unwrap();

        let mut rhs = parse_unary_expr(tokens)?;
        lookahead = tokens.peek()?;

        while let Some(lookahead_op) = BinOperator::from_token_type(&lookahead.token_type) {
            if !precedence_compare(lookahead_op, op) {
                break;
            }

            rhs = parse_bin_expr(tokens, rhs, lookahead_op.precedence())?;
            lookahead = tokens.peek()?;
        }

        lhs = Expr::Binary(op, box lhs, box rhs);
    }

    Ok(lhs)
}

fn parse_scoped_block<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
) -> Result<ScopedBlock> {
    tokens.move_required(&TokenType::Grammar(Grammar::OpenBrace))?;

    let mut vec: Vec<AstNode> = Vec::new();

    loop {
        match tokens.peek()?.token_type {
            TokenType::Grammar(Grammar::OpenBrace) => {
                vec.push(AstNode::ScopedBlock(parse_scoped_block(tokens)?))
            }

            TokenType::Grammar(Grammar::CloseBrace) => {
                tokens.next().unwrap();
                break;
            }

            _ => vec.push(AstNode::Expr(parse_expr(tokens)?)),
        };
    }

    Ok(ScopedBlock(vec.into_boxed_slice()))
}

fn next_ident<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<Ident> {
    let tok = tokens.next()?;
    match tok.token_type {
        TokenType::Ident(ref s) => Ok(Ident(s.clone())),
        _ => Err(Error::IncorrectToken {
            span: tok.span,
            expected: TokenType::Ident("".to_string()),
            actual: tok.token_type.clone(),
        }),
    }
}

fn parse_fn<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<AstNode> {
    tokens.move_required(&TokenType::Keyword(Keyword::Function))?;
    let name = next_ident(tokens)?;
    tokens.move_required(&TokenType::Grammar(Grammar::OpenParen))?;

    let mut args: Vec<(Ident, Ident)> = Vec::new();

    if let TokenType::Ident(_) = tokens.peek()?.token_type {
        loop {
            args.push(parse_var_with_type(tokens)?);
            if tokens.peek()?.token_type == TokenType::Grammar(Grammar::Comma) {
                tokens.next().unwrap();
            } else {
                break;
            }
        }
    }

    tokens.move_required(&TokenType::Grammar(Grammar::CloseParen))?;

    let return_type = tokens
        .peeking_next(|tok| tok.token_type == TokenType::Grammar(Grammar::Arrow))
        .map(|_| next_ident(tokens))
        .transpose()?;

    Ok(AstNode::Function(Function::new(
        FunctionHeader::new(name, args.into_boxed_slice(), return_type),
        parse_scoped_block(tokens)?,
    )))
}

fn parse_mod<T: Iterator<Item = Token>>(mut tokens: TokenIterator<T>) -> Result<AstNode> {
    let mut vec: Vec<AstNode> = Vec::new();
    while let Ok(token) = tokens.peek() {
        match token.token_type {
            TokenType::Keyword(Keyword::Function) => vec.push(parse_fn(&mut tokens)?),
            _ => return Err(Error::UnexpectedToken(token.span, token.token_type.clone())),
        };
    }

    Ok(AstNode::Mod(vec.into_boxed_slice()))
}
