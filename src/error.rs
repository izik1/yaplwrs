use super::{lexer, parser};
use crate::util;
use std::convert;

#[derive(Debug, Clone)]
pub struct Span {
    pub message: String,
    pub span: util::Span,
}

impl Span {
    pub fn new(message: String, span: util::Span) -> Self {
        Self { message, span }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    Lexer(lexer::Error),
    Parser(parser::Error),
    Span(Span),
    String(String),
    Other(&'static str),
    ICE(Option<Span>),
}

impl convert::From<lexer::Error> for Error {
    fn from(e: lexer::Error) -> Self {
        Error::Lexer(e)
    }
}

impl convert::From<parser::Error> for Error {
    fn from(e: parser::Error) -> Self {
        Error::Parser(e)
    }
}

impl convert::From<Span> for Error {
    fn from(e: Span) -> Self {
        Error::Span(e)
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;
