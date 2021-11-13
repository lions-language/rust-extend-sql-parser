use log::debug;
use std::fmt;

use crate::dialect::{Dialect, Keyword};
use crate::tokenizer::{TokenizerError, Token, Tokenizer};
use crate::ast::{Statement};

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
    pub fn parse_sql(dialect: &dyn Dialect, sql: &str) -> Result<Vec<Statement>, ParserError> {
        let mut tokenizer = Tokenizer::new(dialect, sql);
        let tokens = tokenizer.tokenize()?;
        let mut parser = Parser::new(tokens, dialect);
        let mut stmts = Vec::new();
        let mut expecting_statement_delimiter = false;
        debug!("Parseing sql '{}'...", sql);
        loop {
            while parser.consume_token(&Token::SemiColon) {
                expecting_statement_delimiter = false;
            }

            if parser.peek_token() == Token::EOF {
                break;
            }
            if expecting_statement_delimiter {
                return parser.expected("end of statement", parser.peek_token());
            }

            let statement = parser.parse_statement()?;
            stmts.push(statement);
            expecting_statement_delimiter = true;
        }
        Ok(stmts)
    }

    pub fn parse_statement(&mut self) -> Result<Statement, ParserError> {
        match self.next_token() {
            Token::Word(w) => match w.keyword {
                Keyword::EXPLAIN => Ok(self.parse_explain()?),
                Keyword::ANALYZE => Ok(self.parse_analyze()?),
                _ => self.expected("an SQL statement", Token::Word(w)),
            },
            unexpected => self.expected("an SQL statement", unexpected),
        }
    }

    pub fn parse_analyze(&mut self) -> Result<Statement, ParserError> {
        self.expect_keyword(Keyword::TABLE)?;
        let table_name = self.parse_object_name()?;
    }

    pub fn parse_explain(&mut self) -> Result<Statement, ParserError> {
        /*
         * NOTE: analyze verbose [statement]
         * */
        let analyze = self.parse_keyword(Keyword::ANALYZE);
        let verbose = self.parse_keyword(Keyword::VERBOSE);

        let statement = Box::new(self.parse_statement()?);

        Ok(Statement::Explain {
            analyze,
            verbose,
            statement,
        })
    }
}

impl<'a> Parse<'a> {
    pub fn parse_object_name(&mut self) -> Result<ObjectName, ParserError> {
        let mut idents = vec![];
        loop {
            idents.push(self.parse_identifier()?);
            if !self.consume_token(&Token::Period) {
                break;
            }
        }
        Ok(ObjectName(idents))
    }

    pub fn parse_identifier(&mut self) -> Result<Ident, ParserError> {
        match self.next_token() {
            Token::Word(w) => Ok(w.to_ident()),
            Token::SingleQuotedString(s) => Ok(Ident::with_quote('\'', s)),
            Token::BackQuotedString(s) => Ok(Ident::with_quote('`', s)),
            unexpected => self.expected("identifier", unexpected),
        }
    }

    pub fn expect_keyword(&mut self, expected: Keyword) -> Result<(), ParserError> {
        if self.parse_keyword(expected) {
            Ok(())
        } else {
            self.expected(format!("{:?}", &expected).as_str(), self.peek_token())
        }
    }

    pub fn parse_keyword(&mut self, expected: Keyword) -> bool {
        match self.peek_token() {
            Token::Word(w) if expected == w.keyword => {
                self.next_token();
                true
            },
            _ => false,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn peek_nth_token(&self, mut n: usize) -> Token {
        let mut index = self.index;
        loop {
            index += 1;
            /*
             * NOTE: 使用 先+1, 再 -1 的方式, 解决了 边界问题
             * */
            match self.tokens.get(index - 1) {
                Some(Token::Whitespace(_)) => continue,
                non_whitespace => {
                    if n == 0 {
                        return non_whitespace.cloned().unwrap_or(Token::EOF);
                    }
                    n -= 1;
                }
            }
        }
    }

    pub fn peek_token(&self) -> Token {
        self.peek_nth_token(0)
    }

    pub fn next_token(&mut self) -> Token {
        loop {
            self.index += 1;
            match self.tokens.get(self.index - 1) {
                Some(Token::Whitespace(_)) => {
                    continue;
                },
                non_whitespace => {
                    return non_whitespace.cloned().unwrap_or(Token::EOF);
                }
            }
        }
    }

    pub fn next_token_no_skip(&mut self) -> Option<&Token> {
        self.index += 1;
        self.tokens.get(self.index - 1)
    }

    pub fn prev_token(&mut self) {
        loop {
            assert!(self.index > 0);
            self.index += 1;
            if let Some(Token::Whitespace(_)) = self.tokens.get(self.index) {
                continue;
            }
            return;
        }
    }

    pub fn expected<T>(&self, expected: &str, found: Token) -> Result<T, ParserError> {
        parser_err!(format!("Expected {}, found: {}", expected, found))
    }

    pub fn consume_token(&mut self, expected: &Token) -> bool {
        if self.peek_token() == *expected {
            self.next_token();
            true
        } else {
            false
        }
    }
    
    pub fn expect_token(&mut self, expected: &Token) -> Result<(), ParserError> {
        if self.consume_token(expected) {
            Ok(())
        } else {
            self.expected(&expected.to_string(), self.peek_token())
        }
    }

    fn maybe_parse<T, F>(&mut self, mut f: F) -> Option<T>
        where
            F: FnMut(&mut Parser) -> Result<T, ParserError> {
        let index = self.index;
        if let Ok(t) = f(self) {
            Some(t)
        } else {
            self.index = index;
            None
        }
    }

    pub fn new(tokens: Vec<Token>, dialect: &'a dyn Dialect) -> Self {
        Self {
            tokens: tokens,
            index: 0,
            dialect
        }
    }
}

