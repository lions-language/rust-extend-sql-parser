use log::debug;
use std::fmt;

use crate::dialect::{Dialect, Keyword};
use crate::tokenizer::{TokenizerError, Token, Tokenizer, Word};
use crate::ast::*;

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
        let mut for_columns = false;
        let mut cache_metadata = false;
        let mut noscan = false;
        let mut partitions = None;
        let mut compute_statistics = false;
        let mut columns = vec![];
        loop {
            match self.parse_one_of_keywords(&[
                Keyword::PARTITION,
                Keyword::FOR,
                Keyword::CACHE,
                Keyword::NOSCAN,
                Keyword::COMPUTE,
            ]) {
                Some(Keyword::PARTITION) => {
                    self.expect_token(&Token::LParen)?;
                    partitions = Some(self.parse_comma_separated(Parser::parse_expr)?);
                    self.expect_token(&Token::RParen)?;
                },
                Some(Keyword::NOSCAN) => {
                    noscan = true;
                },
                Some(Keyword::FOR) => {
                    self.expect_keyword(Keyword::COLUMNS)?;

                    columns = self.maybe_parse(|parser| {
                        parser.parse_comma_separated(Parser::parse_identifier)
                    }).unwrap_or_default();
                    for_columns = true;
                },
                Some(Keyword::CACHE) => {
                    self.expect_keyword(Keyword::METADATA)?;
                    cache_metadata = true;
                },
                Some(Keyword::COMPUTE) => {
                    self.expect_keyword(Keyword::STATISTICS)?;
                    compute_statistics = true;
                },
                _ => break,
            }
        }

        Ok(Statement::Analyze {
            table_name,
            for_columns,
            columns,
            partitions,
            cache_metadata,
            noscan,
            compute_statistics,
        })
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

    pub fn parse_prefix(&mut self) -> Result<Expr, ParserError> {
        return_ok_if_some!(self.maybe_parse(|parser| {
            match parser.parse_data_type()? {
                DataType::Interval => parser.parse_literal_interval(),
                DataType::Custom(..) => parser_err!("dummy"),
                data_type => Ok(Expr::TypedString {
                    data_type,
                    value: parser.parse_literal_string()?,
                }),
            }
        }));

        let expr = match self.next_token() {
            Token::Word(w) => match w.keyword {
                Keyword::TRUE | Keyword::FALSE | Keyword::NULL => {
                    self.prev_token();
                    Ok(Expr::Value(self.parse_value()?))
                },
            }
        };
    }
    
    pub fn parse_expr(&mut self) -> Result<Expr, ParserError> {
        self.parse_subexpr(0)
    }

    pub fn parse_subexpr(&mut self, precedence: u8) -> Result<Expr, ParserError> {
        debug!("parsing expr");
        let mut expr = self.parse_prefix()?;
        debug!("prefix: {:?}", expr);
        loop {
            let next_precedence = self.get_next_precedence()?;
            debug!("next precedence: {:?}", next_precedence);

            if precedence >= next_precedence {
                break;
            }

            expr = self.parse_infix(expr, next_precedence);
        }

        Ok(expr)
    }

    pub fn parse_comma_separated<T, F>(&mut self, mut f: F) -> Result<Vec<T>, ParserError>
    where
        F: FnMut(&mut Parser<'a>) -> Result<T, ParserError> {
        let mut values = vec![];
        loop {
            values.push(f(self)?);
            if !self.consume_token(&Token::Comma) {
                break;
            }
        }
        Ok(values)
    }

    pub fn parse_literal_uint(&mut self) -> Result<u64, ParserError> {
        match self.next_token() {
            Token::Number(s, _) => s.parse::<u64>().map_err(|e| {
                ParserError::ParserError(format!("Could not parse '{}' as u64: {}", s, e))
            }),
            unexpected => self.expected("literal int", unexpected),
        }
    }

    pub fn parse_literal_string(&mut self) -> Result<String, ParserError> {
        match self.next_token() {
            Token::Word(Word {
                value,
                keyword,
                ..
            }) if keyword == Keyword::NoKeyword => Ok(value),
            Token::SingleQuotedString(s) => Ok(s),
            unexpected => self.expected("literal string", unexpected),
        }
    }

    pub fn parse_literal_interval(&mut self) -> Result<Expr, ParserError> {
        let value = self.parse_literal_string()?;

        let leading_field = match self.peek_token() {
            Token::Word(kw)
                if [
                    Keyword::YEAR,
                    Keyword::MONTH,
                    Keyword::DAY,
                    Keyword::HOUR,
                    Keyword::MINUTE,
                    Keyword::SECOND
                ]
                .iter()
                .any(|d| kw.keyword == *d) => {
                Some(self.parse_date_time_field()?)
            },
            _ => None
        };

        let (leading_precision, last_field, fsec_precision) =
            if leading_field == Some(DateTimeField::Second) {
                let last_field = None;
                let (leading_precision, fsec_precision) = self.parse_optional_precision_scale()?;
                (leading_precision, last_field, fsec_precision)
            } else {
                let leading_precision = self.parse_optional_precision()?;
                if self.parse_keyword(Keyword::TO) {
                    let last_field = Some(self.parse_date_time_field()?);
                    let fsec_precision = if last_field == Some(DateTimeField::Second) {
                        self.parse_optional_precision()?
                    } else {
                        None
                    };
                    (leading_precision, last_field, fsec_precision)
                } else {
                    (leading_precision, None, None)
                }
            };

        Ok(Expr::Value(Value::Interval {
            value,
            leading_field,
            leading_precision,
            last_field,
            fractional_seconds_precision: fsec_precision,
        }))
    }

    pub fn parse_date_time_field(&mut self) -> Result<DateTimeField, ParserError> {
        match self.next_token() {
            Token::Word(w) => match w.keyword {
                Keyword::YEAR => Ok(DateTimeField::Year),
                Keyword::MONTH => Ok(DateTimeField::Month),
                Keyword::DAY => Ok(DateTimeField::Day),
                Keyword::HOUR => Ok(DateTimeField::Hour),
                Keyword::MINUTE => Ok(DateTimeField::Minute),
                Keyword::SECOND => Ok(DateTimeField::Second),
                _ => self.expected("date/time field", Token::Word(w))?,
            },
            unexpected => self.expected("date/time field", unexpected),
        }
    }

    pub fn parse_optional_precision(&mut self) -> Result<Option<u64>, ParserError> {
        if self.consume_token(&Token::LParen) {
            let n = self.parse_literal_uint()?;
            self.expect_token(&Token::RParen)?;
            Ok(Some(n))
        } else {
            Ok(None)
        }
    }

    pub fn parse_optional_precision_scale(
        &mut self,
    ) -> Result<(Option<u64>, Option<u64>), ParserError> {
        if self.consume_token(&Token::LParen) {
            let n = self.parse_literal_uint()?;
            let scale = if self.consume_token(&Token::Comma) {
                Some(self.parse_literal_uint()?)
            } else {
                None
            };
            self.expect_token(&Token::RParen)?;
            Ok((Some(n), scale))
        } else {
            Ok((None, None))
        }
    }
}

impl<'a> Parser<'a> {
    pub fn parse_data_type(&mut self) -> Result<DataType, ParserError> {
        match self.next_token() {
            Token::Word(w) => match w.keyword {
                Keyword::BOOLEAN => Ok(DataType::Boolean),
                Keyword::FLOAT => Ok(DataType::Float(self.parse_optional_precision()?)),
                Keyword::REAL => Ok(DataType::Real),
                Keyword::DOUBLE => {
                    let _ = self.parse_keyword(Keyword::PRECISION);
                    Ok(DataType::Double)
                },
                Keyword::TINYINT => Ok(DataType::TinyInt),
                Keyword::SMALLINT => Ok(DataType::SmallInt),
                Keyword::INT | Keyword::INTEGER => Ok(DataType::Int),
                Keyword::BIGINT => Ok(DataType::BigInt),
                Keyword::VARCHAR => Ok(DataType::Varchar(self.parse_optional_precision()?)),
                Keyword::CHAR | Keyword::CHARACTER => {
                    if self.parse_keyword(Keyword::VARYING) {
                        Ok(DataType::Varchar(self.parse_optional_precision()?))
                    } else {
                        Ok(DataType::Char(self.parse_optional_precision_scale()?))
                    }
                },
                Keyword::UUID => Ok(DataType::Uuid),
                Keyword::DATE => Ok(DataType::Date),
                Keyword::TIMESTAMP => {
                    if self.parse_keyword(Keyword::WITH) || self.parse_keyword(Keyword::WITHOUT) {
                        self.expect_keyword(&[Keyword::TIME, Keyword::ZONE])?;
                    }
                    Ok(DataType::Timestamp)
                },
                Keyword::TIME => {
                    if self.parse_keyword(Keyword::WITH) || self.parse_keyword(Keyword::WITHOUT) {
                        self.expect_keywords(&[Keyword::TIME, Keyword::ZONE])?;
                    }
                    Ok(DataType::Time)
                },
                Keyword::INTERVAL => Ok(DataType::Interval),
                Keyword::REGCLASS => Ok(DataType::Regclass),
                Keyword::STRING => Ok(DataType::String),
                Keyword::TEXT => {
                    if self.consume_token(&Token::LBracket) {
                        self.expect_token(&Token::RBracket)?;
                        Ok(DataType::Array(Box::new(DataType::Text)))
                    } else {
                        Ok(DataType::Text)
                    }
                },
                Keyword::BYTEA => Ok(DataType::Bytea),
                Keyword::NUMERIC | Keyword::DECIMAL | Keyword::DEC => {
                    let (precision, scale) = self.parse_optional_precision_scale()?;
                    Ok(DataType::Decimal(precision, scale))
                },
                _ => {
                    self.prev_token();
                    let type_name = self.parse_object_name()?;
                    Ok(DataType::Custom(type_name))
                }
            },
            unexpected => self.expected("a data type name", unexpected),
        }
    }

    pub fn parse_one_of_keywords(&mut self, keywords: &[Keyword]) -> Option<Keyword> {
        match self.peek_token() {
            Token::Word(w) => {
                keywords
                    .iter()
                    .find(|keyword| **keyword == w.keyword)
                    .map(|keyword| {
                        // consume token
                        self.next_token();
                        *keyword
                    })
            },
            _ => None,
        }
    }

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

    pub fn expect_keywords(&mut self, expected: &[Keyword]) -> Result<(), ParserError> {
        for &kw in expected {
            self.expect_keyword(kw)?
        }
        Ok(())
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

impl Word {
    pub fn to_ident(&self) -> Ident {
        Ident {
            value: self.value.clone(),
            quote_style: self.quote_style,
        }
    }
}

