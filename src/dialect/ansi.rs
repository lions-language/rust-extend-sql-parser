use crate::dialect::Dialect;

#[derive(Debug)]
pub struct AnsiDialect {}

impl Dialect for AnsiDialect {
    fn is_identifier_start(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch)
    }

    fn is_identifier_part(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch)
            || ('A'..='Z').contains(&ch)
            || ('0'..='9').contains(&ch)
            || ch == '_'
    }
}

