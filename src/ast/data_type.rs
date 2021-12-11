#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use std::fmt;

use super::ObjectName;

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Date,
    Time,
    Timestamp,
    Interval,
    Regclass,
    Text,
    String,
    Bytea,
    Custom(ObjectName),
    Array(Box<DataType>)
}

impl fmt::Display for DataType {
    // TODO: write with macro
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DataType::Char(size) => {
                format_type_with_optional_length(f, "CHAR", size)
            },
            DataType::Varchar(size) => {
                format_type_with_optional_length(f, "Varchar", size)
            },
            DataType::Uuid => {
                write!(f, "UUID")
            },
            DataType::Binary(size) => {
                format_type_with_length(f, "Binary", size)
            },
            DataType::Varbinary(size) => {
                format_type_with_length(f, "Varbinary", size)
            },
            _ => {
                unimplemented!();
            }
        }
    }
}

#[inline]
fn format_type_with_optional_length(
    f: &mut fmt::Formatter,
    sql_type: &'static str,
    len: &Option<u64>,
) -> fmt::Result {
    write!(f, "{}", sql_type)?;
    if let Some(len) = len {
        write!(f, "({})", len)?;
    };
    Ok(())
}

#[inline]
fn format_type_with_length(
    f: &mut fmt::Formatter,
    sql_type: &'static str,
    len: &u64,
) -> fmt::Result {
    write!(f, "{}({})", sql_type, len)
}
