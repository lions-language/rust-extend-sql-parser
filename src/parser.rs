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

#[derive(PartialEq)]
pub enum IsOptional {
    Optional,
    Mandator
}

use IsOptional::*;

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
                Keyword::CASE => self.parse_case_expr(),
                Keyword::CAST => self.parse_cast_expr(),
                Keyword::TRY_CAST => self.parse_try_cast_expr(),
                Keyword::EXISTS => self.parse_exists_expr(),
            }
        };
    }
    
    pub fn parse_exists_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect_token(&Token::LParen)?;
        let exists_node = Expr::Exists(Box::new(self.parse_query()?));
        self.expect_token(&Token::RParen)?;
        Ok(exists_node)
    }

    pub fn parse_try_cast_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect_token(&Token::LParen)?;
        let expr = self.parse_expr()?;
        if !self.consume_token(&Token::Comma) {
            self.expect_keyword(Keyword::AS)?;
        }
        let data_type = self.parse_data_type()?;
        self.expect_token(&Token::RParen)?;
        Ok(Expr::TryCast {
            expr: Box::new(expr),
            data_type,
        })
    }

    pub fn parse_case_expr(&mut self) -> Result<Expr, ParserError> {
        let mut operand = None;
        if !self.parse_keyword(Keyword::WHEN) {
            operand = Some(Box::new(self.parse_expr()?));
            self.expect_keyword(Keyword::WHEN)?
        }
        let mut conditions = vec![];
        let mut results = vec![];
        loop {
            conditions.push(self.parse_expr()?);
            self.expect_keyword(Keyword::THEN);
            results.push(self.parse_expr()?);
            if !self.parse_keyword(Keyword::WHEN) {
                break;
            }
        }
        let else_result = if self.parse_keyword(Keyword::ELSE) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        self.expect_keyword(Keyword::END)?;
        Ok(Expr::Case {
            operand,
            conditions,
            results,
            else_result,
        })
    }

    pub fn parse_cast_expr(&mut self) -> Result<Expr, ParserError> {
        self.expect_token(&Token::LParen)?;
        let expr = self.parse_expr()?;
        if !self.consume_token(&Token::Comma) {
            self.expect_keyword(Keyword::AS)?;
        }
        let data_type = self.parse_data_type()?;
        self.expect_token(&Token::RParen)?;
        Ok(Expr::Cast {
            expr: Box::new(expr),
            data_type,
        })
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

            expr = self.parse_infix(expr, next_precedence)?;
        }

        Ok(expr)
    }

    fn parse_value(&mut self) -> Result<Value, ParserError> {
        match self.next_token() {
            Token::Word(w) => match w.keyword {
                Keyword::TRUE => Ok(Value::Boolean(true)),
                Keyword::FALSE => Ok(Value::Boolean(false)),
                Keyword::NULL => Ok(Value::Null),
                Keyword::NoKeyword if w.quote_style.is_some() => match w.quote_style {
                    Some('"') => Ok(Value::DoubleQuotedString(w.value)),
                    Some('\'') => Ok(Value::SingleQuotedString(w.value)),
                    _ => self.expected("A value?", Token::Word(w))?,
                },
                _ => self.expected("a concrete value", Token::Word(w)),
            },
            Token::Number(ref n, l) => match n.parse() {
                Ok(n) => Ok(Value::Number(n, l)),
                Err(e) => parser_err!(format!("Could not parse '{}' as number: {}", n, e)),
            },
            Token::SingleQuotedString(ref s) => Ok(Value::SingleQuotedString(s.to_string())),
            Token::NationalStringLiteral(ref s) => Ok(Value::NationalStringLiteral(s.to_string())),
            Token::HexStringLiteral(ref s) => Ok(Value::HexStringLiteral(s.to_string())),
            unexpected => self.expected("a value", unexpected),
        }
    }

    pub fn parse_in(&mut self, expr: Expr, negated: bool) -> Result<Expr, ParserError> {
        self.expect_token(&Token::LParen)?;
        let in_op = if self.parse_keyword(Keyword::SELECT) || self.parse_keyword(Keyword::WITH) {
            self.prev_token();
            Expr::InSubquery {
                expr: Box::new(expr),
                subquery: Box::new(self.parse_query()?),
                negated,
            }
        } else {
            Expr::InList {
                expr: Box::new(expr),
                list: self.parse_comma_separated(Parser::parse_expr)?,
                negated,
            }
        };
        self.expect_token(&Token::RParen)?;
        Ok(in_op)
    }

    pub fn parse_between(&mut self, expr: Expr, negated: bool) -> Result<Expr, ParserError> {
        let low = self.parse_subexpr(Self::BETWEEN_PREC)?;
        self.expect_keyword(Keyword::AND)?;
        let high = self.parse_subexpr(Self::BETWEEN_PREC)?;
        Ok(Expr::Between {
            expr: Box::new(expr),
            negated,
            low: Box::new(low),
            high: Box::new(high),
        })
    }

    pub fn parse_infix(&mut self, expr: Expr, precedence: u8) -> Result<Expr, ParserError> {
        let tok = self.next_token();
        let regular_binary_operator = match &tok {
            Token::Spaceship => Some(BinaryOperator::Spaceship),
            Token::DoubleEq => Some(BinaryOperator::Eq),
            Token::Eq => Some(BinaryOperator::Eq),
            Token::Neq => Some(BinaryOperator::NotEq),
            Token::Gt => Some(BinaryOperator::Gt),
            Token::GtEq => Some(BinaryOperator::GtEq),
            Token::Lt => Some(BinaryOperator::Lt),
            Token::LtEq => Some(BinaryOperator::LtEq),
            Token::Plus => Some(BinaryOperator::Plus),
            Token::Minus => Some(BinaryOperator::Minus),
            Token::Mult => Some(BinaryOperator::Multiply),
            Token::Mod => Some(BinaryOperator::Modulus),
            Token::StringConcat => Some(BinaryOperator::StringConcat),
            Token::Pipe => Some(BinaryOperator::BitwiseOr),
            Token::Caret => Some(BinaryOperator::BitwiseXor),
            Token::Ampersand => Some(BinaryOperator::BitwiseAnd),
            Token::Div => Some(BinaryOperator::Divide),
            // Token::ShiftLeft if dialect_of!(self is PostgresqlDialect) => {
            //     Some(BinaryOperator::PGBitwiseShiftLeft)
            // },
            // Token::ShiftRight if dialect_of!(self is PostgresqlDialect) => {
            //     Some(BinaryOperator::PGBitwiseShiftRight)
            // },
            // Token::Shape if dialect_of!(self is PostgresqlDialect) => {
            //     Some(BinaryOperator::PGBitwiseXor)
            // },
            Token::Word(w) => match w.keyword {
                Keyword::AND => Some(BinaryOperator::And),
                Keyword::OR => Some(BinaryOperator::Or),
                Keyword::LIKE => Some(BinaryOperator::Like),
                Keyword::ILIKE => Some(BinaryOperator::ILike),
                Keyword::NOT => {
                    if self.parse_keyword(Keyword::LIKE) {
                        Some(BinaryOperator::NotLike)
                    } else if self.parse_keyword(Keyword::ILIKE) {
                        Some(BinaryOperator::NotILike)
                    } else {
                        None
                    }
                },
                _ => None,
            },
            _ => None,
        };

        if let Some(op) = regular_binary_operator {
            Ok(Expr::BinaryOp {
                left: Box::new(expr),
                op,
                right: Box::new(self.parse_subexpr(precedence)?),
            })
        } else if let Token::Word(w) = &tok {
            match w.keyword {
                Keyword::IS => {
                    if self.parse_keyword(Keyword::NULL) {
                        Ok(Expr::IsNull(Box::new(expr)))
                    } else if self.parse_keywords(&[Keyword::NOT, Keyword::NULL]) {
                        Ok(Expr::IsNotNull(Box::new(expr)))
                    } else {
                        self.expected("NULL or NOT NULL after IS", self.peek_token())
                    }
                },
                Keyword::NOT | Keyword::IN | Keyword::BETWEEN => {
                    self.prev_token();
                    let negated = self.parse_keyword(Keyword::NOT);
                    if self.parse_keyword(Keyword::IN) {
                        self.parse_in(expr, negated)
                    } else if self.parse_keyword(Keyword::BETWEEN) {
                        self.parse_between(expr, negated)
                    } else {
                        self.expected("IN or BETWEEN after NOT", self.peek_token())
                    }
                },
                _ => parser_err!(format!("No infix parser for token {:?}", tok)),
            }
        } else if Token::DoubleColon == tok {
            // self.parse_pg_cast(expr)
            unimplemented!();
        } else if Token::ExclamationMark == tok {
            unimplemented!();
            // Ok(Expr::UnaryOp {
            //     op: UnaryOperator::PGPostfixFactorial,
            //     expr: Box::new(expr)
            // })
        } else if Token::LBracket == tok {
            self.parse_map_access(expr)
        } else {
            parser_err!(format!("No infix parser for token {:?}", tok))
        }
    }

    pub fn parse_query(&mut self) -> Result<Query, ParserError> {
        let with = if self.parse_keyword(Keyword::WITH) {
            Some(With {
                recursive: self.parse_keyword(Keyword::RECURSIVE),
                cte_tables: self.parse_comma_separated(Parser::parse_cte)?,
            })
        } else {
            None
        };

        if !self.parse_keyword(Keyword::INSERT) {
            let body = self.parse_query_body(0)?;

            let order_by = if self.parse_keywords(&[Keyword::ORDER, Keyword::BY]) {
                self.parse_comma_separated(Parser::parse_order_by_expr)?
            } else {
                vec![]
            };

            let limit = if self.parse_keyword(Keyword::LIMIT) {
                self.parse_limit()?
            } else {
                None
            };

            let offset = if self.parse_keyword(Keyword::OFFSET) {
                Some(self.parse_offset()?)
            } else {
                None
            };

            let fetch = if self.parse_keyword(Keyword::FETCH) {
                Some(self.parse_fetch()?)
            } else {
                None
            };

            Ok(Query {
                with,
                body,
                order_by,
                limit,
                offset,
                fetch,
            })
        } else {
            let insert = self.parse_insert()?;
            Ok(Query {
                with,
                body: SetExpr::Insert(insert),
                limit: None,
                order_by: vec![],
                offset: None,
                fetch: None,
            })
        }
    }

    pub fn parse_order_by_expr(&mut self) -> Result<OrderByExpr, ParserError> {
        let expr = self.parse_expr()?;

        let asc = if self.parse_keyword(Keyword::ASC) {
            Some(true)
        } else if self.parse_keyword(Keyword::DESC) {
            Some(false)
        } else {
            None
        };

        let nulls_first = if self.parse_keywords(&[Keyword::NULLS, Keyword::FIRST]) {
            Some(true)
        } else if self.parse_keywords(&[Keyword::NULLS, Keyword::LAST]) {
            Some(false)
        } else {
            Some(false)
        };

        Ok(OrderByExpr {
            expr,
            asc,
            nulls_first,
        })
    }

    fn parse_cte(&mut self) -> Result<Cte, ParserError> {
        let name = self.parse_identifier()?;

        let mut cte = if self.parse_keyword(Keyword::AS) {
            self.expect_token(&Token::LParen)?;
            let query = self.parse_query()?;
            self.expect_token(&Token::RParen)?;
            let alias = TableAlias {
                name,
                columns: vec![],
            };
            Cte {
                alias,
                query,
                from: None,
            }
        } else {
            let columns = self.parse_parenthesized_column_list(Optional)?;
            self.expect_keyword(Keyword::AS)?;
            self.expect_token(&Token::LParen)?;
            let query = self.parse_query()?;
            self.expect_token(&Token::RParen)?;
            let alias = TableAlias { name, columns };
            Cte {
                alias,
                query,
                from: None,
            }
        };

        if self.parse_keyword(Keyword::FROM) {
            cte.from = Some(self.parse_identifier()?);
        }
        Ok(cte)
    }

    pub fn parse_parenthesized_column_list(&mut self, optional: IsOptional)
        -> Result<Vec<Ident>, ParserError> {
        if self.consume_token(&Token::LParen) {
            let cols = self.parse_comma_separated(Parser::parse_identifier)?;
            self.expect_token(&Token::RParen)?;
            Ok(cols)
        } else if optional == Optional {
            Ok(vec![])
        } else {
            self.expected("a list of columns in parenthese", self.peek_token())
        }
    }

    pub fn parse_query_body(&mut self, precedence: u8) -> Result<SetExpr, ParserError> {
        let mut expr = if self.parse_keyword(Keyword::SELECT) {
            SetExpr::Select(Box::new(self.parse_select()?))
        } else if self.consume_token(&Token::LParen) {
            let subquery = self.parse_query()?;
            self.expect_token(&Token::RParen)?;
            SetExpr::Query(Box::new(subquery))
        } else if self.parse_keyword(Keyword::VALUES) {
            SetExpr::Values(self.parse_values()?)
        } else {
            return self.expected(
                "SELECT, VALUES, or a subquery in the query body",
                self.peek_token(),
            );
        };

        loop {
            let op = self.parse_set_operator(&self.peek_token());
            let next_precedence = match op {
                Some(SetOperator::Union) | Some(SetOperator::Except) => 10,
                Some(SetOperator::Intersect) => 20,
                None => break,
            };
            if precedence >= next_precedence {
                break;
            }
            self.next_token();
            expr = SetExpr::SetOperation {
                left: Box::new(expr),
                op: op.unwrap(),
                all: self.parse_keyword(Keyword::ALL),
                right: Box::new(self.parse_query_body(next_precedence)?),
            };
        }

        Ok(expr)
    }

    pub fn parse_set_operator(&mut self, token: &Token) -> Option<SetOperator> {
        match token {
            Token::Word(w) if w.keyword == Keyword::UNION => Some(SetOperator::Union),
            Token::Word(w) if w.keyword == Keyword::EXCEPT => Some(SetOperator::Except),
            Token::Word(w) if w.keyword == Keyword::INTERSECT => Some(SetOperator::Intersect),
            _ => None,
        }
    }

    pub fn parse_map_access(&mut self, expr: Expr)  -> Result<Expr, ParserError> {
        let key = self.parse_literal_string()?;
        let tok = self.consume_token(&Token::RBracket);
        debug!("Tok: {}", tok);
        match expr {
            e @ Expr::Identifier(_) | e @ Expr::CompoundIdentifier(_) => Ok(Expr::MapAccess {
                column: Box::new(e),
                key,
            }),
            _ => Ok(expr),
        }
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

    pub fn parse_values(&mut self) -> Result<Values, ParserError> {
        let values = self.parse_comma_separated(|parser| {
            parser.expect_token(&Token::LParen)?;
            let exprs = parser.parse_comma_separated(Parser::parse_expr)?;
            parser.expect_token(&Token::RParen)?;
            Ok(exprs)
        })?;
        Ok(Values(values))
    }

    pub fn parse_number_value(&mut self) -> Result<Value, ParserError> {
        match self.parse_value()? {
            v @ Value::Number(_, _) => Ok(v),
            _ => {
                self.prev_token();
                self.expected("literal number", self.peek_token())
            }
        }
    }

    pub fn parse_limit(&mut self) -> Result<Option<Expr>, ParserError> {
        if self.parse_keyword(Keyword::ALL) {
            Ok(None)
        } else {
            Ok(Some(Expr::Value(self.parse_number_value()?)))
        }
    }

    pub fn parse_offset(&mut self) -> Result<Offset, ParserError> {
        let value = Expr::Value(self.parse_number_value()?);
        let rows = if self.parse_keyword(Keyword::ROW) {
            OffsetRows::Row
        } else if self.parse_keyword(Keyword::ROWS) {
            OffsetRows::Rows
        } else {
            OffsetRows::None
        };
        Ok(Offset { value, rows })
    }

    pub fn parse_fetch(&mut self) -> Result<Fetch, ParserError> {
        self.expect_one_of_keywords(&[Keyword::FIRST, Keyword::NEXT])?;
        let (quantity, percent) = if self
            .parse_one_of_keywords(&[Keyword::ROW, Keyword::ROWS])
            .is_some() {
            (None, false)
        } else {
            let quantity = Expr::Value(self.parse_value()?);
            let percent = self.parse_keyword(Keyword::PERCENT);
            self.expect_one_of_keywords(&[Keyword::ROW, Keyword::ROWS])?;
            (Some(quantity), percent)
        };
        let with_ties = if self.parse_keyword(Keyword::ONLY) {
            false
        } else if self.parse_keywords(&[Keyword::WITH, Keyword::TIES]) {
            true
        } else {
            return self.expected("one of ONLY or WITH TIES", self.peek_token());
        };
        Ok(Fetch {
            with_ties,
            percent,
            quantity,
        })
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
                        Ok(DataType::Char(self.parse_optional_precision()?))
                    }
                },
                Keyword::UUID => Ok(DataType::Uuid),
                Keyword::DATE => Ok(DataType::Date),
                Keyword::TIMESTAMP => {
                    if self.parse_keyword(Keyword::WITH) || self.parse_keyword(Keyword::WITHOUT) {
                        self.expect_keywords(&[Keyword::TIME, Keyword::ZONE])?;
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

    const UNARY_NOT_PREC: u8 = 15;
    const BETWEEN_PREC: u8 = 20;
    const PLUS_MINUS_PREC: u8 = 30;

    pub fn get_next_precedence(&self) -> Result<u8, ParserError> {
        let token = self.peek_token();
        debug!("get_next_precedence() {:?}", token);
        match token {
            Token::Word(w) if w.keyword == Keyword::OR => Ok(5),
            Token::Word(w) if w.keyword == Keyword::AND => Ok(10),
            Token::Word(w) if w.keyword == Keyword::NOT => match self.peek_nth_token(1) {
                Token::Word(w) if w.keyword == Keyword::IN => Ok(Self::BETWEEN_PREC),
                Token::Word(w) if w.keyword == Keyword::BETWEEN => Ok(Self::BETWEEN_PREC),
                Token::Word(w) if w.keyword == Keyword::LIKE => Ok(Self::BETWEEN_PREC),
                Token::Word(w) if w.keyword == Keyword::ILIKE => Ok(Self::BETWEEN_PREC),
                _ => Ok(0),
            },
            Token::Word(w) if w.keyword == Keyword::IS => Ok(17),
            Token::Word(w) if w.keyword == Keyword::IN => Ok(Self::BETWEEN_PREC),
            Token::Word(w) if w.keyword == Keyword::BETWEEN => Ok(Self::BETWEEN_PREC),
            Token::Word(w) if w.keyword == Keyword::LIKE => Ok(Self::BETWEEN_PREC),
            Token::Word(w) if w.keyword == Keyword::ILIKE => Ok(Self::BETWEEN_PREC),
            Token::Eq
            | Token::Lt
            | Token::LtEq
            | Token::Neq
            | Token::Gt
            | Token::GtEq
            | Token::DoubleEq
            | Token::Spaceship => Ok(20),
            Token::Pipe => Ok(21),
            Token::Caret | Token::Sharp | Token::ShiftRight | Token::ShiftLeft => Ok(22),
            Token::Ampersand => Ok(23),
            Token::Plus | Token::Minus => Ok(Self::PLUS_MINUS_PREC),
            Token::Mult | Token::Div | Token::Mod | Token::StringConcat => Ok(40),
            Token::DoubleColon => Ok(50),
            Token::ExclamationMark => Ok(50),
            Token::LBracket | Token::RBracket => Ok(10),
            _ => Ok(0),
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

    pub fn parse_keywords(&mut self, keywords: &[Keyword]) -> bool {
        let index = self.index;
        for &keyword in keywords {
            if !self.parse_keyword(keyword) {
                self.index = index;
                return false;
            }
        }
        true
    }

    pub fn expect_one_of_keywords(&mut self, keywords: &[Keyword]) -> Result<Keyword, ParserError> {
        if let Some(keyword) = self.parse_one_of_keywords(keywords) {
            Ok(keyword)
        } else {
            let keywords: Vec<String> = keywords.iter().map(|x| format!("{:?}", x)).collect();
            self.expected(
                &format!("one of {}", keywords.join(" or ")),
                self.peek_token(),
            )
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
            self.index -= 1;
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

