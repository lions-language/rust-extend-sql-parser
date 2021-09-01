#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use std::fmt;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum DataType {
    /// Fixed-length character type e.g. CHAR(10) / CHAR()
    Char(Option<u64>),
    /// Variable-length character type e.g. VARCHAR(10) / VARCHAR()
    Varchar(Option<u64>),
    /// Uuid type
    Uuid,
    /// Large character object e.g. CLOB(1000)
    Clob(u64),
    Binary(u64),
    Varbinary(u64),
    Blob(u64),
    Decimal(Option<u64>, Option<u64>),
    Float(Option<u64>),
    TinyInt,
    SmallInt,
    Int,
    BigInt,
    Real,
    Double,
    Boolean,
    Data,
    Time,
    Timestamp,
    Interval,
    Regclass,
    Text,
    String,
    Bytea,
    // Custom(ObjectName)
    Array(Box<DataType>)
}

impl fmt::Display for DataType {
    fn fmt(&self, f: fmt::Formatter) -> fmt::Result {
        match self {
            DataType::Char(size) => {
            },
            _ => {
            }
        }
    }
}

fn format_type_with_optional_length(
    f: &mut fmt::Formatter,
    sql_type: &'static str,
    len: &Option<u64>,
) -> fmt::Result {
    write!(f, "{}", sql_type)?;
}
