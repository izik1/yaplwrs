use crate::ast;
use crate::token::{self, Token};
use crate::util::Span;
use itertools::PeekingNext;
use std::iter::Peekable;

#[derive(Debug, Clone)]
pub enum Error {
    OutOfTokens,
    ReservedToken(String, Span),
    IncorrectToken {
        span: Span,
        expected: token::Kind,
        actual: token::Kind,
    },
    InvalidPrimaryExpression(Span),
    UnexpectedToken(Span, token::Kind),
}

type Result<T> = ::std::result::Result<T, Error>;

struct TokenIterator<T: Iterator<Item = Token>> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> TokenIterator<T> {
    pub fn new(tokens: T) -> Self {
        Self {
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

    pub fn move_required(&mut self, token_type: &token::Kind) -> Result<()> {
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

pub fn parse<T: Iterator<Item = Token>>(tokens: T) -> Result<ast::Node> {
    parse_mod(TokenIterator::new(tokens))
}

fn parse_unary_expr<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<ast::Expr> {
    let token = tokens.peek()?;

    if let Some(op) = ast::UnaryOperator::from_token_type(&token.token_type) {
        tokens.next()?;
        return Ok(ast::Expr::Unary(op, box parse_unary_expr(tokens)?));
    }

    match token.token_type {
        token::Kind::Grammar(token::Grammar::Plus) => {
            Err(Error::ReservedToken("Unary Plus".to_string(), token.span))
        }
        _ => Ok(ast::Expr::Primary(parse_primary(tokens)?)),
    }
}

// NOTE: this assumes that the caller already ate the "if" token, and doesn't check for it.
fn parse_if<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<ast::If> {
    let cond = parse_expr(tokens)?;
    let block = parse_scoped_block(tokens)?;
    let mut elseifs = vec![];
    let block_else = loop {
        if let Some(_) = tokens.peeking_next(|tok| tok.token_type == token::Keyword::Else.into()) {
            if let Some(_) = tokens.peeking_next(|tok| tok.token_type == token::Keyword::If.into())
            {
                elseifs.push(ast::ElseIf(
                    box parse_expr(tokens)?,
                    parse_scoped_block(tokens)?,
                ));
            } else {
                break Some(parse_scoped_block(tokens)?);
            }
        } else {
            break None;
        }
    };

    Ok(ast::If(
        box cond,
        block,
        elseifs.into_boxed_slice(),
        block_else,
    ))
}

fn parse_var_with_type<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
) -> Result<ast::FunctionArg> {
    let ident = next_ident(tokens)?;
    tokens.move_required(&token::Kind::Grammar(token::Grammar::Colon))?;
    Ok(ast::FunctionArg { ident, ty: next_ident(tokens)? })
}

fn parse_call<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
    id: ast::Ident,
) -> Result<ast::Primary> {
    tokens.move_required(&token::Kind::Grammar(token::Grammar::OpenParen))?;
    let mut args = vec![];
    while !tokens
        .peeking_next(|tok| tok.token_type == token::Kind::Grammar(token::Grammar::CloseParen))
        .is_some()
    {
        args.push(parse_expr(tokens)?);
        tokens.peeking_next(|tok| tok.token_type == token::Kind::Grammar(token::Grammar::Comma));
    }

    Ok(ast::Primary::FunctionCall(id, args.into_boxed_slice()))
}

fn parse_primary<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<ast::Primary> {
    let token = tokens.next()?;
    match token.token_type {
        token::Kind::Ident(ref id) => {
            let id = ast::Ident(id.clone());
            if tokens.peek()?.token_type == token::Kind::Grammar(token::Grammar::OpenParen) {
                parse_call(tokens, id)
            } else {
                Ok(ast::Primary::Ident(id))
            }
        }

        token::Kind::Integer(ref i, ref suffix) => {
            Ok(ast::Primary::Integer(i.clone(), suffix.clone()))
        }
        token::Kind::Keyword(token::Keyword::If) => Ok(ast::Primary::If(parse_if(tokens)?)),
        _ => Err(Error::InvalidPrimaryExpression(token.span)),
    }
}

fn parse_expr<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<ast::Expr> {
    let lhs = parse_unary_expr(tokens)?;
    parse_bin_expr(tokens, lhs, 0)
}

fn parse_bin_expr<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
    mut lhs: ast::Expr,
    min_priority: usize,
) -> Result<ast::Expr> {
    fn precedence_compare(a: ast::BinOperator, b: ast::BinOperator) -> bool {
        a.precedence() > b.precedence()
            || (a.precedence() == b.precedence()
                && (a.associativity() == ast::Associativity::Right))
    }

    let mut lookahead = tokens.peek()?;
    while let Some(op) = ast::BinOperator::from_token_type(&lookahead.token_type) {
        if op.precedence() < min_priority {
            break;
        }

        tokens.next().unwrap();

        let mut rhs = parse_unary_expr(tokens)?;
        lookahead = tokens.peek()?;

        while let Some(lookahead_op) = ast::BinOperator::from_token_type(&lookahead.token_type) {
            if !precedence_compare(lookahead_op, op) {
                break;
            }

            rhs = parse_bin_expr(tokens, rhs, lookahead_op.precedence())?;
            lookahead = tokens.peek()?;
        }

        lhs = ast::Expr::Binary(op, box lhs, box rhs);
    }

    Ok(lhs)
}

fn parse_scoped_block<T: Iterator<Item = Token>>(
    tokens: &mut TokenIterator<T>,
) -> Result<ast::ScopedBlock> {
    tokens.move_required(&token::Kind::Grammar(token::Grammar::OpenBrace))?;

    let mut vec: Vec<ast::Node> = Vec::new();

    loop {
        match tokens.peek()?.token_type {
            token::Kind::Grammar(token::Grammar::OpenBrace) => {
                vec.push(ast::Node::ScopedBlock(parse_scoped_block(tokens)?))
            }

            token::Kind::Grammar(token::Grammar::CloseBrace) => {
                tokens.next().unwrap();
                break;
            }

            // todo: Allow for stuff in the format: `{ expr; expr; expr; }` all on the same line
            // this is the only way to tell `{ ident + ident; (expr) }` from `ident + call(expr)`
            // this should _deny_ code that looks like this: `expr expr` or `expr {}`
            _ => vec.push(ast::Node::Expr(parse_expr(tokens)?)),
        };
    }

    Ok(ast::ScopedBlock(vec.into_boxed_slice()))
}

fn next_ident<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<ast::Ident> {
    let tok = tokens.next()?;
    match tok.token_type {
        token::Kind::Ident(ref s) => Ok(ast::Ident(s.clone())),
        _ => Err(Error::IncorrectToken {
            span: tok.span,
            expected: token::Kind::Ident("".to_string()),
            actual: tok.token_type.clone(),
        }),
    }
}

fn parse_fn<T: Iterator<Item = Token>>(tokens: &mut TokenIterator<T>) -> Result<ast::Node> {
    tokens.move_required(&token::Kind::Keyword(token::Keyword::Function))?;
    let name = next_ident(tokens)?;
    tokens.move_required(&token::Kind::Grammar(token::Grammar::OpenParen))?;

    let mut args: Vec<ast::FunctionArg> = Vec::new();

    if let token::Kind::Ident(_) = tokens.peek()?.token_type {
        loop {
            args.push(parse_var_with_type(tokens)?);
            if tokens.peek()?.token_type == token::Kind::Grammar(token::Grammar::Comma) {
                tokens.next().unwrap();
            } else {
                break;
            }
        }
    }

    tokens.move_required(&token::Kind::Grammar(token::Grammar::CloseParen))?;

    let return_type = tokens
        .peeking_next(|tok| tok.token_type == token::Kind::Grammar(token::Grammar::Arrow))
        .map(|_| next_ident(tokens))
        .transpose()?;

    Ok(ast::Node::Function(ast::Function::new(
        ast::FunctionHeader::new(name, args.into_boxed_slice(), return_type),
        parse_scoped_block(tokens)?,
    )))
}

fn parse_mod<T: Iterator<Item = Token>>(mut tokens: TokenIterator<T>) -> Result<ast::Node> {
    let mut vec: Vec<ast::Node> = Vec::new();
    while let Ok(token) = tokens.peek() {
        match token.token_type {
            token::Kind::Keyword(token::Keyword::Function) => vec.push(parse_fn(&mut tokens)?),
            _ => return Err(Error::UnexpectedToken(token.span, token.token_type.clone())),
        };
    }

    Ok(ast::Node::Mod(vec.into_boxed_slice()))
}
