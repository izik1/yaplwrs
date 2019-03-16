use super::{lexer, parser};
use crate::util::Span;
use std::convert;

#[derive(Debug, Clone)]
pub struct SpanError {
    pub message: String,
    pub span: Span,
}

impl SpanError {
    pub fn new(message: String, span: Span) -> Self {
        Self { message, span }
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    Lexer(lexer::Error),
    Parser(parser::Error),
    Span(SpanError),
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

impl convert::From<SpanError> for Error {
    fn from(e: SpanError) -> Self {
        Error::Span(e)
    }
}

pub type Result<T> = ::std::result::Result<T, Error>;
