mod data_type;
mod ddl;
mod operator;
mod value;
mod query;

use std::fmt;

pub use self::data_type::DataType;
pub use value::DateTimeField;
use self::ddl::ColumnDef;
pub(crate) use self::value::{Value};
pub use self::operator::{UnaryOperator, BinaryOperator};
pub use self::query::{Query, Values, Top, SelectItem, TableWithJoins, LateralView,
                    With, Select, SetExpr, SetOperator, Cte, TableAlias, OrderByExpr,
                    Offset, OffsetRows, Fetch};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        assert!(quote == '\'' || quote == '"' || quote == '`' || quote == '[');
        Ident {
            value: value.into(),
            quote_style: Some(quote)
        }
    }
}

impl From<&str> for Ident {
    fn from(value: &str) -> Self {
        Self {
            value: value.to_string(),
            quote_style: None
        }
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.quote_style {
            Some(q) if q == '"' || q == '\'' || q == '`' => {
                write!(f, "{}{}{}", q, self.value, q)
            },
            Some(q) if q == '[' => {
                write!(f, "[{}]", self.value)
            },
            None => {
                f.write_str(&self.value)
            },
            _ => {
                panic!("unexpected quote style");
            }
        }
    }
}

//////////////////////////////////
// possibly multi-part, i.e. db.schema.obj
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ObjectName(pub Vec<Ident>);

impl fmt::Display for ObjectName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", display_separated(&self.0, "."))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ListAgg {
    pub distinct: bool,
    pub expr: Box<Expr>,
    pub separator: Option<Box<Expr>>,
    pub on_overflow: Option<ListAggOnOverflow>,
    pub within_group: Vec<OrderByExpr>,
}

impl fmt::Display for ListAgg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "LISTAGG({}{}",
               if self.distinct { "DISTINCT" } else { "" },
               self.expr)?;
        if let Some(separator) = &self.separator {
            write!(f, ", {}", separator)?;
        }
        if let Some(on_overflow) = &self.on_overflow {
            write!(f, "{}", on_overflow)?;
        }
        write!(f, ")")?;
        if !self.within_group.is_empty() {
            write!(f,
                   " WITHIN GROUP (ORDER BY {})",
                   display_comma_separated(&self.within_group))?;
        }
        Ok(())
    }
}

//////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    Identifier(Ident),
    CompoundIdentifier(Vec<Ident>),
    BinaryOp {
        op: BinaryOperator,
        left: Box<Expr>,
        right: Box<Expr>
    },
    UnaryOp {
        op: UnaryOperator,
        expr: Box<Expr>
    },
    Value(Value),
    TypedString {
        data_type: DataType,
        value: String
    },
    MapAccess {
        column: Box<Expr>,
        key: String
    },
    IsNull(Box<Expr>),
    IsNotNull(Box<Expr>),
    InList {
        expr: Box<Expr>,
        list: Vec<Expr>,
        negated: bool,
    },
    InSubquery {
        expr: Box<Expr>,
        subquery: Box<Query>,
        negated: bool,
    },
    Between {
        expr: Box<Expr>,
        negated: bool,
        low: Box<Expr>,
        high: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        data_type: DataType,
    },
    Case {
        operand: Option<Box<Expr>>,
        conditions: Vec<Expr>,
        results: Vec<Expr>,
        else_result: Option<Box<Expr>>
    },
    TryCast {
        expr: Box<Expr>,
        data_type: DataType,
    },
    Exists(Box<Query>),
    Substring {
        expr: Box<Expr>,
        substring_from: Option<Box<Expr>>,
        substring_for: Option<Box<Expr>>,
    },
    Extract {
        field: DateTimeField,
        expr: Box<Expr>,
    },
    ListAgg(ListAgg),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Identifier(s) => {
                write!(f, "{}", s)
            },
            Expr::BinaryOp {
                op,
                left,
                right,
            } => {
                write!(f, "{} {} {}", op, left, right)
            },
            Expr::UnaryOp {
                op,
                expr,
            } => {
                write!(f, "{} {}", op, expr)
            }
        }
    }
}

//////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionArg {
    Named {
        name: Ident,
        arg: Expr,
    },
    Unnamed(Expr),
}

impl fmt::Display for FunctionArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use FunctionArg::*;
        
        match self {
            Named {
                name,
                arg
            } => {
                write!(f, "{} => {}", name, arg)
            },
            Unnamed(unnamed_arg) => {
                write!(f, "{}", unnamed_arg)
            }
        }
    }
}

//////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SqlOption {
    pub name: Ident,
    pub value: Value
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Statement {
    Analyze {
        table_name: ObjectName,
        partitions: Option<Vec<Expr>>,
        for_columns: bool,
        columns: Vec<Ident>,
        cache_metadata: bool,
        noscan: bool,
        compute_statistics: bool
    },
    CreateTable {
        name: String,
        columns: Vec<ColumnDef>,
        with_options: Vec<SqlOption>
    },
    Explain {
        analyze: bool,
        verbose: bool,
        statement: Box<Statement>,
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
            },
            Statement::Explain {
                verbose,
                analyze,
                statement,
            } => {
                write!(f, "EXPLAIN ")?;

                if *analyze {
                    write!(f, "ANALYZE ")?;
                }

                if *verbose {
                    write!(f, "VERBOSE ")?;
                }

                write!(f, "{}", statement)
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

