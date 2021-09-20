use std::fmt;

pub enum Value {
    Number(String, bool),
    // Number(BigDecimal, bool),
    SingleQuotedString(String),
    NationalStringLiteral(String),
    HexStringLiteral(String),
    DoubleQuotedString(String),
    Boolean(bool),
    Interval{
        value: String,
        leading_field: Option<DateTimeField>,
        leading_precision: Option<u64>,
        last_field: Option<DateTimeField>,
        fractional_seconds_precision: Option<u64>
    },
    Null
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Value::*;

        match self {
            Number(v, l) => {
                write!(f, "{}{long}", v, long = if *l { "L" } else { "" })
            },
            SingleQuotedString(v) => {
                write!(f, "\"{}\"", v)
            },
            NationalStringLiteral(v) => {
                write!(f, "N'{}'", v)
            },
            HexStringLiteral(v) => {
                write!(f, "X'{}'", v)
            },
            DoubleQuotedString(v) => {
                write!(f, "{}", v)
            },
            Boolean(v) => {
                write!(f, "{}", v)
            },
            Interval {
                value,
                leading_field: Some(DateTimeField::Second),
                leading_precision: Some(leading_precision),
                last_field,
                fractional_seconds_precision: Some(fractional_seconds_precision)
            } => {
                assert!(last_field.is_none());
                write!(f, "INTERVAL '{}' SECOND ({}, {})", escape_single_quote_string(value), leading_precision
                       , fractional_seconds_precision)
            },
            Null => {
            }
        }
    }
}

////////////////////////////////
pub enum DateTimeField {
    Year,
    Month,
    Day,
    Hour,
    Minute,
    Second
}

impl fmt::Display for DateTimeField {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            DateTimeField::Year => "YEAR",
            DateTimeField::Month => "MONTH",
            DateTimeField::Day => "DAY",
            DateTimeField::Hour => "HOUR",
            DateTimeField::Minute => "MINUTE",
            DateTimeField::Second => "SECOND"
        })
    }
}
