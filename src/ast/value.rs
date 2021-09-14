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
            StringQuotedString(v) => {
                write!(f, "\"{}\"", v)
            },
            NationalStringLiteral(v) => {
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
