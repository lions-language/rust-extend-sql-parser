mod data_type;
mod ddl;
mod operator;
mod value;
mod query;

use std::fmt;

pub use self::data_type::DataType;
pub use value::DateTimeField;
use self::ddl::ColumnDef;
pub(crate) use self::value::Value;
pub use self::operator::{UnaryOperator, BinaryOperator};

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
        subquery: Box<Expr>,
        negated: bool,
    },
    Between {
        expr: Box<Expr>,
        negated: bool,
        low: Box<Expr>,
        high: Box<Expr>,
    }
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

//////////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SetExpr {
    Select(Box<Select>),
    Query(Box<Query>),
    SetOperation {
        op: SetOperator,
        all: bool,
        left: Box<SetExpr>,
        right: Box<SetExpr>
    },
    Values(Values),
    Insert(Statement),
}

impl fmt::Display for SetExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SetExpr::Select(s) => write!(f, "{}", s),
            SetExpr::Query(s) => write!(f, "({})", q),
            SetExpr::Values(v) => write!(f, "{}", v),
            SetExpr::Insert(v) => write!(f, "{}", v),
            SetExpr::SetOperation => {
                left,
                right,
                op,
                all,
            } => {
                let all_str = if *all { " ALL" } else { "" };
                write!(f, "{} {}{} {}", left, op, all_str, right);
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SetOperator {
    Union,
    Except,
    Intersect,
}

impl fmt::Display for SetOperator {
    fn fmt(&self, f: &mut fmt::Formattor) -> fmt::Result {
        f.write_str(match self {
            SetOperator::Union => "UNION",
            SetOperator::Except => "EXCEPT",
            SetOperator::Intersect => "INTERSECT",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Select {
    pub distinct: bool,
    pub top: Option<Top>,
    pub projection: Vec<SelectItem>,
    pub from: Vec<TableWithJoins>,
    pub lateral_views: Vec<LateralView>,
    pub selection: Option<Expr>,
    pub group_by: Vec<Expr>,
    pub cluster_by: Vec<Expr>,
    pub distinct_by: Vec<Expr>,
    pub sort_by: Vec<Expr>,
    pub having: Option<Expr>
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    }
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

