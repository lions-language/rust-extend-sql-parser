mod data_type;
mod ddl;
mod value;

use std::fmt;

use self::data_type::DataType;
use self::ddl::ColumnDef;
use self::value::Value;

pub struct Ident {
    pub value: String,
    // ' or " or ` or [
    pub quote_style: Option<char>
}

impl Ident {
    pub fn new<S>(value: S) -> Self
    where
        S: Into<String>,
    {
        Ident {
            value: value.into(),
            quote_style: None
        }
    }

    pub fn with_quote<S>(quote: char, value: S) -> Self
    where
        S: Into<String>,
    {
        assert!(quote == '\'' || quote == '"' || quote == '`', quote == '[');
        Ident {
            value: value.into(),
            quote_style: Some(quote)
        }
    }
}

//////////////////////////////////
// possibly multi-part, i.e. db.schema.obj
pub struct ObjectName(pub Vec<Ident>);

pub struct SqlOption {
    pub name: Ident,
    pub value: Value
}

pub enum Statement {
    CreateTable {
        name: String,
        columns: Vec<ColumnDef>,
        with_options: Vec<SqlOption>
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::CreateTable {
                name,
                columns,
                with_options,
            } => {
                write!(
                    f,
                    "CREATE TABLE {name}",
                    name = name
                )?;

                Ok(())
            }
        }
    }
}

//////////////////////////////////
struct DisplaySeparated<'a, T>
where
    T: fmt::Display,
{
    slice: &'a [T],
    sep: &'static str
}

impl<'a, T> fmt::Display for DisplaySeparated<'a, T>
where
    T: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut delim = "";
        for t in self.slice {
            write!(f, "{}", delim)?;
            delim = self.sep;
            write!(f, "{}", t)?;
        }
        Ok(())
    }
}

fn display_separated<'a, T>(slice: &'a [T], sep: &'static str) -> DisplaySeparated<'a, T>
where
    T: fmt::Display,
{
    DisplaySeparated{
        slice: slice,
        sep: sep
    }
}

fn display_comma_separated<'a, T>(slice: &'a [T]) -> DisplaySeparated<'a, T>
where
    T: fmt::Display,
{
    DisplaySeparated{
        slice: slice,
        sep: ","
    }
}

