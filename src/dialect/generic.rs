use crate::dialect::Dialect;

#[derive(Debug)]
pub struct GenericDialect {
}

impl Dialect for GenericDialect {
    fn is_identifier_start(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch)
            || ('A'..='Z').contains(&ch)
            || ch == '_'
            || ch == '#'
            || ch == '@'
    }

    fn is_identifier_part(&self, ch: char) -> bool {
        ('a'..='z').contains(&ch)
            || ('A'..='Z').contains(&ch)
            || ('0'..='9').contains(&ch)
            || ch == '@'
            || ch == '$'
            || ch == '#'
            || ch == '_'
    }
}
