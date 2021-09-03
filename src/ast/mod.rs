mod data_type;
mod ddl;

use self::data_type::DataType;

pub struct Ident {
    pub value: String,
    // 'xxx'
    pub quote_style: Option<char>
}

pub enum Statement {
    CreateTable {
    }
}

