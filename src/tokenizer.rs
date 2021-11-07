use std::iter::Peekable;
use std::str::Chars;

use std::fmt;

use crate::dialect::keywords::{Keyword, ALL_KEYWORDS, ALL_KEYWORDS_INDEX};
use crate::dialect::Dialect;
use crate::dialect::SnowflakeDialect;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    EOF,
    Word(Word),
    /// An unsigned numeric literal
    Number(String, bool),
    /// A character that could not be tokenized
    Char(char),
    /// Single quoted string: i.e: 'string'
    SingleQuotedString(String),
    BackQuotedString(String),
    /// "National" string literal: i.e: N'string'
    NationalStringLiteral(String),
    /// Hexadecimal string literal: i.e.: X'deadbeef'
    HexStringLiteral(String),
    /// Comma
    Comma,
    /// Whitespace (space, tab, etc)
    Whitespace(Whitespace),
    /// Double equals sign `==`
    DoubleEq,
    /// Equality operator `=`
    Eq,
    /// Not Equals operator `<>` (or `!=` in some dialects)
    Neq,
    /// Less Than operator `<`
    Lt,
    /// Greater Than operator `>`
    Gt,
    /// Less Than Or Equals operator `<=`
    LtEq,
    /// Greater Than Or Equals operator `>=`
    GtEq,
    /// Spaceship operator <=>
    Spaceship,
    /// Plus operator `+`
    Plus,
    /// Minus operator `-`
    Minus,
    /// Multiplication operator `*`
    Mult,
    /// Division operator `/`
    Div,
    /// Modulo Operator `%`
    Mod,
    /// String concatenation `||`
    StringConcat,
    /// Left parenthesis `(`
    LParen,
    /// Right parenthesis `)`
    RParen,
    /// Period (used for compound identifiers or projections into nested types)
    Period,
    /// Colon `:`
    Colon,
    /// DoubleColon `::` (used for casting in postgresql)
    DoubleColon,
    /// SemiColon `;` used as separator for COPY and payload
    SemiColon,
    /// Backslash `\` used in terminating the COPY payload with `\.`
    Backslash,
    /// Left bracket `[`
    LBracket,
    /// Right bracket `]`
    RBracket,
    /// Ampersand `&`
    Ampersand,
    /// Pipe `|`
    Pipe,
    /// Caret `^`
    Caret,
    /// Left brace `{`
    LBrace,
    /// Right brace `}`
    RBrace,
    /// Right Arrow `=>`
    RArrow,
    /// Sharp `#` used for PostgreSQL Bitwise XOR operator
    Sharp,
    /// Tilde `~` used for PostgreSQL Bitwise NOT operator
    Tilde,
    /// `<<`, a bitwise shift left operator in PostgreSQL
    ShiftLeft,
    /// `>>`, a bitwise shift right operator in PostgreSQL
    ShiftRight,
    /// Exclamation Mark `!` used for PostgreSQL factorial operator
    ExclamationMark,
    /// Double Exclamation Mark `!!` used for PostgreSQL prefix factorial operator
    DoubleExclamationMark,
    /// AtSign `@` used for PostgreSQL abs operator
    AtSign,
    PGSquareRoot,
    PGCubeRoot
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::EOF => f.write_str("EOF"),
            Token::Word(ref w) => write!(f, "{}", w),
            Token::Number(ref n, l) => write!(f, "{}{long}", n, long = if *l { "L" } else { "" }),
            Token::Char(ref c) => write!(f, "{}", c),
            Token::SingleQuotedString(ref s) => write!(f, "'{}'", s),
            Token::BackQuotedString(ref s) => write!(f, "`{}`", s),
            Token::NationalStringLiteral(ref s) => write!(f, "N'{}'", s),
            Token::HexStringLiteral(ref s) => write!(f, "X'{}'", s),
            Token::Comma => f.write_str(","),
            Token::Whitespace(ws) => write!(f, "{}", ws),
            Token::DoubleEq => f.write_str("=="),
            Token::Spaceship => f.write_str("<=>"),
            Token::Eq => f.write_str("="),
            Token::Neq => f.write_str("<>"),
            Token::Lt => f.write_str("<"),
            Token::Gt => f.write_str(">"),
            Token::LtEq => f.write_str("<="),
            Token::GtEq => f.write_str(">="),
            Token::Plus => f.write_str("+"),
            Token::Minus => f.write_str("-"),
            Token::Mult => f.write_str("*"),
            Token::Div => f.write_str("/"),
            Token::StringConcat => f.write_str("||"),
            Token::Mod => f.write_str("%"),
            Token::LParen => f.write_str("("),
            Token::RParen => f.write_str(")"),
            Token::Period => f.write_str("."),
            Token::Colon => f.write_str(":"),
            Token::DoubleColon => f.write_str("::"),
            Token::SemiColon => f.write_str(";"),
            Token::Backslash => f.write_str("\\"),
            Token::LBracket => f.write_str("["),
            Token::RBracket => f.write_str("]"),
            Token::Ampersand => f.write_str("&"),
            Token::Caret => f.write_str("^"),
            Token::Pipe => f.write_str("|"),
            Token::LBrace => f.write_str("{"),
            Token::RBrace => f.write_str("}"),
            Token::RArrow => f.write_str("=>"),
            Token::Sharp => f.write_str("#"),
            Token::ExclamationMark => f.write_str("!"),
            Token::DoubleExclamationMark => f.write_str("!!"),
            Token::Tilde => f.write_str("~"),
            Token::AtSign => f.write_str("@"),
            Token::ShiftLeft => f.write_str("<<"),
            Token::ShiftRight => f.write_str(">>"),
            Token::PGSquareRoot => f.write_str("|/"),
            Token::PGCubeRoot => f.write_str("||/"),
        }
    }
}

impl Token {
    pub fn make_keyword(keyword: &str) -> Self {
        Token::make_word(keyword, None)
    }

    pub fn make_word(word: &str, quote_style: Option<char>) -> Self {
        let word_uppercase = word.to_uppercase();
        Token::Word(Word {
            value: word.to_string(),
            quote_style,
            keyword: if quote_style == None {
                let keyword = ALL_KEYWORDS.binary_search(&word_uppercase.as_str());
                keyword.map_or(Keyword::NoKeyword, |x| ALL_KEYWORDS_INDEX[x])
            } else {
                Keyword::NoKeyword
            },
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Word {
    pub value: String,
    pub quote_style: Option<char>,
    pub keyword: Keyword
}

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.quote_style {
            Some(s) if s == '"' || s == '{' || s == '`'  => {
                write!(f, "{}{}{}", s, self.value, Word::matching_end_quote(s))
            },
            None => {
                f.write_str(&self.value)
            },
            // if s != " && s != { && s != `
            _ => panic!("Unexpected quote_style!"),
        }
    }
}

impl Word {
    // " after "
    // [ after ]
    // ` after `
    fn matching_end_quote(ch: char) -> char {
        match ch {
            '"' => '"', // ANSI and more dialects
            '[' => ']', // MS SQL
            '`' => '`', // MySQL
            _ => panic!("unexpected quoting style!"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Whitespace {
    Space,
    Newline,
    Tab,
    SingleLineComment { comment: String, prefix: String },
    MultiLineComment(String),
}

impl fmt::Display for Whitespace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Whitespace::*;
        match self {
            Space => f.write_str(" "),
            Newline => f.write_str("\n"),
            Tab => f.write_str("\t"),
            SingleLineComment {
                prefix,
                comment,
            } => write!(f, "{}{}", prefix, comment),
            MultiLineComment(s) => write!(f, "/*{}*/", s),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TokenizerError {
    pub message: String,
    pub line: u64,
    pub col: u64
}

/*
 * NOTE
 * A Tokenizer is loaded with only one query statement
 * => line: The total number of rows in a query statement
 *    col: Record the column where the token is in error
 * */
pub struct Tokenizer<'a> {
    dialect: &'a dyn Dialect,
    pub query: String,
    pub line: u64,
    pub col: u64,
}

impl<'a> Tokenizer<'a> {
    pub fn new(dialect: &'a dyn Dialect, query: &str) -> Self {
        Self {
            dialect: dialect,
            query: query.to_string(),
            line: 1,
            col: 1,
        }
    }

    fn consume_and_return(
        &self,
        chars: &mut Peekable<Chars<'_>>,
        t: Token,
    ) -> Result<Option<Token>, TokenizerError> {
        chars.next();
        Ok(Some(t))
    }

    #[inline]
    fn consume(
        &self,
        chars: &mut Peekable<Chars<'_>>,
    ) {
        chars.next();
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenizerError> {
        let mut peekable = self.query.chars().peekable();

        let mut tokens: Vec<Token> = vec![];

        while let Some(token) = self.next_token(&mut peekable)? {
            match &token {
                Token::Whitespace(Whitespace::Newline) => {
                    self.line += 1;
                    self.col = 1;
                },
                Token::Whitespace(Whitespace::Tab) => self.col += 4,
                Token::Word(w) if w.quote_style == None => self.col += w.value.len() as u64,
                Token::Word(w) if w.quote_style != None => self.col += w.value.len() as u64 + 2,
                Token::Number(s, _) => self.col += s.len() as u64,
                Token::SingleQuotedString(s) => self.col += s.len() as u64,
                _ => self.col += 1,
            }

            tokens.push(token);
        }
        Ok(tokens)
    }

    fn next_token(&self, chars: &mut Peekable<Chars<'_>>) -> Result<Option<Token>, TokenizerError> {
        use Whitespace::*;
        match chars.peek() {
            Some(&ch) => match ch {
                ' ' => self.consume_and_return(chars, Token::Whitespace(Space)),
                '\t' => self.consume_and_return(chars, Token::Whitespace(Tab)),
                '\n' => self.consume_and_return(chars, Token::Whitespace(Newline)),
                '\r' => {
                    self.consume(chars);
                    if let Some('\n') = chars.peek() {
                        self.consume(chars);
                    };
                    Ok(Some(Token::Whitespace(Newline)))
                },
                'N' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('\'') => {
                            let s = self.tokenize_single_quoted_string(chars)?;
                            Ok(Some(Token::NationalStringLiteral(s)))
                        },
                        _ => {
                            let s = self.tokenize_word('N', chars);
                            Ok(Some(Token::make_word(&s, None)))
                        }
                    }
                },
                x @ 'x' | x @ 'X' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('\'') => {
                            let s = self.tokenize_single_quoted_string(chars)?;
                            Ok(Some(Token::HexStringLiteral(s)))
                        },
                        _ => {
                            let s = self.tokenize_word(x, chars);
                            Ok(Some(Token::make_word(&s, None)))
                        }
                    }
                },
                ch if self.dialect.is_identifier_start(ch) => {
                    self.consume(chars);
                    let s = self.tokenize_word(ch, chars);

                    /*
                     * NOTE: [hive] is_identifier_start mayby is '0'-'9'
                     *  If all satisfy the lexical of numbers, they need to be treated as numbers
                     * */
                    if s.chars().all(|x| ('0'..='9').contains(&x) || x == '.') {
                        let mut s = peeking_take_while(&mut s.chars().peekable(), |ch| {
                            matches!(ch, '0'..='9' | '.')
                        });
                        /*
                         * NOTE: '.' does not satisfy the lexical rules of identifier, but satisfies the lexical rules of numeric values, so if the last character is'.', the following may also be numeric values
                         * */
                        let s2 = peeking_take_while(chars, |ch| matches!(ch, '0'..='9' | '.'));
                        s += s2.as_str();
                        return Ok(Some(Token::Number(s, false)));
                    }

                    Ok(Some(Token::make_word(&s, None)))
                },
                '\'' => {
                    let s = self.tokenize_single_quoted_string(chars)?;
                    Ok(Some(Token::SingleQuotedString(s)))
                },
                '`' => {
                    let s = self.tokenize_back_quoted_string(chars)?;
                    Ok(Some(Token::BackQuotedString(s)))
                },
                quote_start if self.dialect.is_delimited_identifier_start(quote_start) => {
                    self.consume(chars);
                    let quote_end = Word::matching_end_quote(quote_start);
                    let s = peeking_take_while(chars, |ch| ch != quote_end);
                    if chars.next() == Some(quote_end) {
                        Ok(Some(Token::make_word(&s, Some(quote_start))))
                    } else {
                        self.tokenizer_error(
                            format!("Expected close delimiter '{}' before EOF.",
                                    quote_end).as_str())
                    }
                },
                '0'..='9' | '.' => {
                    /*
                     * NOTE: before .
                     * */
                    let mut s = peeking_take_while(chars, |ch| matches!(ch, '0'..='9'));
                    if let Some('.') = chars.peek() {
                        s.push('.');
                        self.consume(chars);
                    }
                    /*
                     * NOTE: after .
                     * */
                    s += &peeking_take_while(chars, |ch| matches!(ch, '0'..='9'));

                    /*
                     * NOTE: s == complete numeric string, if s == "." => is period token
                     * */
                    if s == "." {
                        return Ok(Some(Token::Period));
                    }

                    /*
                     * NOTE: is numeric
                     * */

                    let long = if chars.peek() == Some(&'L') {
                        chars.next();
                        true
                    } else {
                        false
                    };
                    Ok(Some(Token::Number(s, long)))
                },
                '(' => self.consume_and_return(chars, Token::LParen),
                ')' => self.consume_and_return(chars, Token::RParen),
                ',' => self.consume_and_return(chars, Token::Comma),
                '-' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('-') => {
                            self.consume(chars);
                            let comment = self.tokenize_single_comment(chars);
                            Ok(Some(Token::Whitespace(Whitespace::SingleLineComment {
                                prefix: "--".to_owned(),
                                comment,
                            })))
                        },
                        _ => {
                            /*
                             * NOTE: not '-' / None => Minus
                             * */
                            Ok(Some(Token::Minus))
                        }
                    }
                },
                '/' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('*') => {
                            self.consume(chars);
                            self.tokenize_multiline_comment(chars)
                        },
                        Some('/') if dialect_of!(self is SnowflakeDialect) => {
                            self.consume(chars);
                            let comment = self.tokenize_single_comment(chars);
                            Ok(Some(Token::Whitespace(Whitespace::SingleLineComment {
                                prefix: "//".to_owned(),
                                comment,
                            })))
                        },
                        _ => {
                            Ok(Some(Token::Div))
                        }
                    }
                },
                '+' => self.consume_and_return(chars, Token::Plus),
                '*' => self.consume_and_return(chars, Token::Mult),
                '%' => self.consume_and_return(chars, Token::Mod),
                '|' => {
                    self.consume(chars);
                    match chars.peek() {
                        // |/
                        Some('/') => self.consume_and_return(chars, Token::PGSquareRoot),
                        Some('|') => {
                            self.consume(chars);
                            match chars.peek() {
                                // |//
                                Some('/') => self.consume_and_return(chars, Token::PGCubeRoot),
                                // ||
                                _ => Ok(Some(Token::StringConcat))
                            }
                        },
                        // only |
                        _ => Ok(Some(Token::Pipe)),
                    }
                },
                '=' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('>') => {
                            self.consume_and_return(chars, Token::RArrow)
                        },
                        _ => Ok(Some(Token::Eq))
                    }
                },
                '!' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('=') => self.consume_and_return(chars, Token::Neq),
                        Some('!') => self.consume_and_return(chars, Token::DoubleExclamationMark),
                        _ => Ok(Some(Token::ExclamationMark))
                    }
                },
                '<' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('=') => {
                            self.consume(chars);
                            match chars.peek() {
                                // <=>
                                Some('>') => self.consume_and_return(chars, Token::Spaceship),
                                _ => Ok(Some(Token::LtEq))
                            }
                        },
                        Some('>') => self.consume_and_return(chars, Token::Neq),
                        Some('<') => self.consume_and_return(chars, Token::ShiftLeft),
                        _ => Ok(Some(Token::Lt))
                    }
                },
                '>' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some('=') => self.consume_and_return(chars, Token::GtEq),
                        Some('>') => self.consume_and_return(chars, Token::ShiftLeft),
                        _ => Ok(Some(Token::Lt))
                    }
                },
                ':' => {
                    self.consume(chars);
                    match chars.peek() {
                        Some(':') => self.consume_and_return(chars, Token::DoubleColon),
                        _ => Ok(Some(Token::Colon))
                    }
                },
                ';' => self.consume_and_return(chars, Token::SemiColon),
                '\\' => self.consume_and_return(chars, Token::Backslash),
                '[' => self.consume_and_return(chars, Token::LBracket),
                ']' => self.consume_and_return(chars, Token::RBracket),
                '&' => self.consume_and_return(chars, Token::Ampersand),
                '^' => self.consume_and_return(chars, Token::Caret),
                '{' => self.consume_and_return(chars, Token::LBrace),
                '}' => self.consume_and_return(chars, Token::RBrace),
                '#' if dialect_of!(self is SnowflakeDialect) => {
                    self.consume(chars);
                    let comment = self.tokenize_single_comment(chars);
                    Ok(Some(Token::Whitespace(Whitespace::SingleLineComment {
                        prefix: "#".to_owned(),
                        comment
                    })))
                },
                '~' => self.consume_and_return(chars, Token::Tilde),
                '#' => self.consume_and_return(chars, Token::Sharp),
                '@' => self.consume_and_return(chars, Token::AtSign),
                other => self.consume_and_return(chars, Token::Char(other))
            },
            None => {
                Ok(None)
            }
        }
    }

    fn tokenizer_error<R>(&self, message: &str) -> Result<R, TokenizerError> {
        Err(TokenizerError {
            message: message.to_string(),
            col: self.col,
            line: self.line
        })
    }

    fn tokenize_word(&self, first_char: char, chars: &mut Peekable<Chars<'_>>) -> String {
        let mut s = first_char.to_string();
        s.push_str(&peeking_take_while(chars, |ch| {
            self.dialect.is_identifier_part(ch)
        }));
        s
    }

    fn tokenize_single_quoted_string(
        &self,
        chars: &mut Peekable<Chars<'_>>,
    ) -> Result<String, TokenizerError> {
        let mut s = String::new();
        self.consume(chars);
        while let Some(&ch) = chars.peek() {
            match ch {
                '\'' => {
                    self.consume(chars);
                    // NOTE: escaped \'
                    let escaped_quote = chars.peek().map(|c| *c == '\'').unwrap_or(false);
                    if escaped_quote {
                        s.push('\'');
                        self.consume(chars);
                    } else {
                        return Ok(s);
                    }
                },
                _ => {
                    s.push(ch);
                    self.consume(chars);
                }
            }
        }
        self.tokenizer_error("Unterminated string literal")
    }

    fn tokenize_back_quoted_string(
        &self,
        chars: &mut Peekable<Chars<'_>>,
    ) -> Result<String, TokenizerError> {
        let mut s = String::new();
        self.consume(chars);
        unimplemented!();
    }

    /*
     * single comment
     * -- xxx
     * -- xx
     * */
    fn tokenize_single_comment(&self, chars: &mut Peekable<Chars<'_>>) -> String {
        let mut comment = peeking_take_while(chars, |ch| ch != '\n');
        if let Some(ch) = chars.next() {
            assert_eq!(ch, '\n');
            comment.push(ch);
        }
        comment
    }

    /*
     * multiline comment
     * */
    fn tokenize_multiline_comment(&self, chars: &mut Peekable<Chars<'_>>) -> Result<Option<Token>, TokenizerError> {
        let mut s = String::new();
        let mut maybe_closing_comment = false;
        loop {
            match chars.next() {
                Some(ch) => {
                    if maybe_closing_comment {
                        if ch == '/' {
                            /*
                             * NOTE: end of multiline comment
                             * */
                            break Ok(Some(Token::Whitespace(Whitespace::MultiLineComment(s))));
                        } else {
                            s.push('*');
                        }
                    }

                    maybe_closing_comment = ch == '*';
                    if !maybe_closing_comment {
                        s.push(ch);
                    }
                },
                None => {
                    break self.tokenizer_error("Unexpected EOF while in a multi-lien comment");
                }
            }
        }
    }
}

fn peeking_take_while(
    chars: &mut Peekable<Chars<'_>>,
    mut predicate: impl FnMut(char) -> bool
) -> String {
    let mut s = String::new();
    while let Some(&ch) = chars.peek() {
        if predicate(ch) {
            chars.next();
            s.push(ch);
        } else {
            break;
        }
    }
    s
}

#[cfg(test)]
mod test {
    use super::super::dialect::GenericDialect;
    use super::super::dialect::MsSqlDialect;
    use super::*;

    #[test]
    fn tokenize_select_1() {
        let sql = String::from("SELECT 1");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from("1"), false),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_select_float() {
        let sql = String::from("SELECT .1");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from(".1"), false),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_scalar_function() {
        let sql = String::from("SELECT sqrt(1)");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("sqrt", None),
            Token::LParen,
            Token::Number(String::from("1"), false),
            Token::RParen,
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_string_string_concat() {
        let sql = String::from("SELECT 'a' || 'b'");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::SingleQuotedString(String::from("a")),
            Token::Whitespace(Whitespace::Space),
            Token::StringConcat,
            Token::Whitespace(Whitespace::Space),
            Token::SingleQuotedString(String::from("b")),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_bitwise_op() {
        let sql = String::from("SELECT one | two ^ three");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("one", None),
            Token::Whitespace(Whitespace::Space),
            Token::Pipe,
            Token::Whitespace(Whitespace::Space),
            Token::make_word("two", None),
            Token::Whitespace(Whitespace::Space),
            Token::Caret,
            Token::Whitespace(Whitespace::Space),
            Token::make_word("three", None),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_simple_select() {
        let sql = String::from("SELECT * FROM customer WHERE id = 1 LIMIT 5");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Mult,
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("FROM"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("customer", None),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("WHERE"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("id", None),
            Token::Whitespace(Whitespace::Space),
            Token::Eq,
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from("1"), false),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("LIMIT"),
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from("5"), false),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_explain_select() {
        let sql = String::from("EXPLAIN SELECT * FROM customer WHERE id = 1");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("EXPLAIN"),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Mult,
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("FROM"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("customer", None),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("WHERE"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("id", None),
            Token::Whitespace(Whitespace::Space),
            Token::Eq,
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from("1"), false),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_explain_analyze_select() {
        let sql = String::from("EXPLAIN ANALYZE SELECT * FROM customer WHERE id = 1");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("EXPLAIN"),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("ANALYZE"),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Mult,
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("FROM"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("customer", None),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("WHERE"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("id", None),
            Token::Whitespace(Whitespace::Space),
            Token::Eq,
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from("1"), false),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_string_predicate() {
        let sql = String::from("SELECT * FROM customer WHERE salary != 'Not Provided'");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Mult,
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("FROM"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("customer", None),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("WHERE"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("salary", None),
            Token::Whitespace(Whitespace::Space),
            Token::Neq,
            Token::Whitespace(Whitespace::Space),
            Token::SingleQuotedString(String::from("Not Provided")),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_invalid_string() {
        let sql = String::from("\nمصطفىh");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        println!("tokens: {:#?}", tokens);
        let expected = vec![
            Token::Whitespace(Whitespace::Newline),
            Token::Char('م'),
            Token::Char('ص'),
            Token::Char('ط'),
            Token::Char('ف'),
            Token::Char('ى'),
            Token::make_word("h", None),
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_newline_in_string_literal() {
        let sql = String::from("'foo\r\nbar\nbaz'");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![Token::SingleQuotedString("foo\r\nbar\nbaz".to_string())];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_unterminated_string_literal() {
        let sql = String::from("select 'foo");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        assert_eq!(
            tokenizer.tokenize(),
            Err(TokenizerError {
                message: "Unterminated string literal".to_string(),
                line: 1,
                col: 8
            })
        );
    }

    #[test]
    fn tokenize_invalid_string_cols() {
        let sql = String::from("\n\nSELECT * FROM table\tمصطفىh");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        println!("tokens: {:#?}", tokens);
        let expected = vec![
            Token::Whitespace(Whitespace::Newline),
            Token::Whitespace(Whitespace::Newline),
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::Mult,
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("FROM"),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("table"),
            Token::Whitespace(Whitespace::Tab),
            Token::Char('م'),
            Token::Char('ص'),
            Token::Char('ط'),
            Token::Char('ف'),
            Token::Char('ى'),
            Token::make_word("h", None),
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_right_arrow() {
        let sql = String::from("FUNCTION(key=>value)");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::make_word("FUNCTION", None),
            Token::LParen,
            Token::make_word("key", None),
            Token::RArrow,
            Token::make_word("value", None),
            Token::RParen,
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_is_null() {
        let sql = String::from("a IS NULL");
        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();

        let expected = vec![
            Token::make_word("a", None),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("IS"),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("NULL"),
        ];

        compare(expected, tokens);
    }

    #[test]
    fn tokenize_comment() {
        let sql = String::from("0--this is a comment\n1");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::Number("0".to_string(), false),
            Token::Whitespace(Whitespace::SingleLineComment {
                prefix: "--".to_string(),
                comment: "this is a comment\n".to_string(),
            }),
            Token::Number("1".to_string(), false),
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_comment_at_eof() {
        let sql = String::from("--this is a comment");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![Token::Whitespace(Whitespace::SingleLineComment {
            prefix: "--".to_string(),
            comment: "this is a comment".to_string(),
        })];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_multiline_comment() {
        let sql = String::from("0/*multi-line\n* /comment*/1");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::Number("0".to_string(), false),
            Token::Whitespace(Whitespace::MultiLineComment(
                "multi-line\n* /comment".to_string(),
            )),
            Token::Number("1".to_string(), false),
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_multiline_comment_with_even_asterisks() {
        let sql = String::from("\n/** Comment **/\n");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::Whitespace(Whitespace::Newline),
            Token::Whitespace(Whitespace::MultiLineComment("* Comment *".to_string())),
            Token::Whitespace(Whitespace::Newline),
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_mismatched_quotes() {
        let sql = String::from("\"foo");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        assert_eq!(
            tokenizer.tokenize(),
            Err(TokenizerError {
                message: "Expected close delimiter '\"' before EOF.".to_string(),
                line: 1,
                col: 1
            })
        );
    }

    #[test]
    fn tokenize_newlines() {
        let sql = String::from("line1\nline2\rline3\r\nline4\r");

        let dialect = GenericDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, &sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::make_word("line1", None),
            Token::Whitespace(Whitespace::Newline),
            Token::make_word("line2", None),
            Token::Whitespace(Whitespace::Newline),
            Token::make_word("line3", None),
            Token::Whitespace(Whitespace::Newline),
            Token::make_word("line4", None),
            Token::Whitespace(Whitespace::Newline),
        ];
        compare(expected, tokens);
    }

    #[test]
    fn tokenize_mssql_top() {
        let sql = "SELECT TOP 5 [bar] FROM foo";
        let dialect = MsSqlDialect {};
        let mut tokenizer = Tokenizer::new(&dialect, sql);
        let tokens = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::make_keyword("SELECT"),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("TOP"),
            Token::Whitespace(Whitespace::Space),
            Token::Number(String::from("5"), false),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("bar", Some('[')),
            Token::Whitespace(Whitespace::Space),
            Token::make_keyword("FROM"),
            Token::Whitespace(Whitespace::Space),
            Token::make_word("foo", None),
        ];
        compare(expected, tokens);
    }

    fn compare(expected: Vec<Token>, actual: Vec<Token>) {
        //println!("------------------------------");
        //println!("tokens   = {:?}", actual);
        //println!("expected = {:?}", expected);
        //println!("------------------------------");
        assert_eq!(expected, actual);
    }
}

