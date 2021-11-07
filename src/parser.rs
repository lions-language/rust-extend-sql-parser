use log::debug;
use std::fmt;

use crate::dialect::Dialect;
use crate::tokenizer::{TokenizerError, Token};

#[derive(Debug)]
pub enum ParserError {
    TokenizerError(String),
    ParserError(String)
}

macro_rules! parser_err {
    ($MSG:expr) => {
        Err(ParserError::ParserError($MSG.to_string()))
    };
}

macro_rules! return_ok_if_some {
    ($e:expr) => {{
        if let Some(v) = $e {
            return Ok(v);
        }
    }};
}

impl From<TokenizerError> for ParserError {
    fn from(e: TokenizerError) -> Self {
        ParserError::TokenizerError(
            format!("{} at Line: {}, Column: {}",
                    e.message, e.line, e.col))
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sql parser error: {}",
            match self {
                ParserError::TokenizerError(e) => e,
                ParserError::ParserError(e) => e,
            }
        )
    }
}

impl std::error::Error for ParserError {
}

pub struct Parser<'a> {
    tokens: Vec<Token>,
    index: usize,
    dialect: &'a dyn Dialect
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, dialect: &'a dyn Dialect) -> Self {
        Self {
            tokens: tokens,
            index: 0,
            dialect
        }
    }
}

