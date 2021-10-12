use std::any::{Any, TypeId};
use std::fmt::Debug;

mod ansi;
mod generic;
mod keywords;
mod sqlite;

/*
 * if obj.dialect.is::<SQLiteDialect>() ||
 *    obj.dialect.is::<MySqlDialect>() ||
 *    obj.dialect.is::<MsSqlDialect>() {
 *    ...
 * }
 * */
macro_rules! dialect_of {
    ( $parsed_dialect:ident is $($dialect_type: ty) | +) => {
        ($($parsed_dialect.dialect.is::<$dialect_type>()) || +)
    };
}

pub trait Dialect : Debug + Any {
    fn is_delimited_identifier_start(&self, ch: char) -> bool {
        ch == '"'
    }
    fn is_identifier_start(&self, ch: char) -> bool;
    fn is_identifier_part(&self, ch: char) -> bool;
}

impl dyn Dialect {
    #[inline]
    pub fn is<T: Dialect>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }
}
