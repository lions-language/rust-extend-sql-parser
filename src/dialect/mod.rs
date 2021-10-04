use std::any::{Any, TypeId};

pub trait Dialect : Debug + Any {
    fn is_delimited_identifier_start(&self, ch: char) -> bool {
        ch == '"'
    }
    fn is_identifier_start(&self, ch: char) -> bool;
    fn is_identifier_part(&self, ch: char) -> bool;
}

impl dyn Dialect {
    pub fn is<T: Dialect>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }
}
