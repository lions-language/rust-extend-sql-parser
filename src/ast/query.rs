use crate::ast::*;

pub struct Query {
    pub with: Option<With>,
    pub body: SetExpr,
}

impl fmt::Display for Query {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(ref with) = self.with {
            write!(f, "{} ", with)?;
        }
        write!(f, "{}", self.body)?;

        Ok(())
    }
}

//////////////////////////////
pub enum SetExpr {
    Select(Box<Select>),
    Query(Box<Query>),
    SetOperation {
        op: SetOperator,
        all: bool,
        left: Box<SetExpr>,
        right: Box<SetExpr>,
    },
    Values(Value),
    Insert(Statement)
}

impl fmt::Display for SetExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SetExpr::*;

        match self {
            Select(s) => write!(f, "{}", s)?,
            Query(q) => write!(f, "{}", q)?,
            Values(v) => write!(f, "{}", v)?,
            Insert(v) => write!(f, "{}", v)?,
            SetOperation {
                op,
                all,
                left,
                right,
            } => {
                let all_str = if *all { " ALL" } else { "" };
                write!(f, "{} {}{} {}", left, op, all_str, right)?;
            }
        }

        Ok(())
    }
}

//////////////////////////////
pub enum SetOperator {
    Union,
    Except,
    Intersect,
}

//////////////////////////////
pub enum Top {
    pub with_ties: bool,
    pub percent: bool,
    pub quantity: Option<Expr>
}

impl fmt::Display for Top {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let extension = if self.with_ties { " WITH TIES" } else { "" };
        if let Some(ref quantity) = self.quantity {
            let percent = if self.percent { " PERCENT" } else { "" };
            write!(f, "TOP ({}){}{}", quantity, percent, extension)?;
        } else {
            write!(f, "TOP{}", extension)?;
        }

        Ok(())
    }
}

//////////////////////////////
pub struct SelectItem {
    UnnamedExpr(Expr),
    ExprWithAlias{
        expr: Expr,
        alias: Ident,
    },
    QualifiedWildcard(ObjectName),
    Wildcard,
}

impl fmt::Display for SelectItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SelectItem::*;

        match self {
            UnnamedExpr(e) => write!(f, "{}", e)?,
            ExprWithAlias{
                expr,
                alias,
            } => {
                write!(f, "{} AS {}", expr, alias)?;
            },
            QualifiedWildcard(prefix) => write!(f, "{}.*", prefix)?,
            Wildcard => write!(f, "*")?,
        }

        Ok(())
    }
}

//////////////////////////////
pub struct Select {
    pub distinct: bool,
    pub top: Option<Top>,
    pub projection: Vec<SelectItem>,
    pub from: Vec<TableWithJoins>,
    pub lateral_views: Vec<LateralView>,
    pub selection: Option<Expr>,
    pub group_by: Vec<Expr>,
    pub cluster_by: Vec<Expr>,
    pub distribute_by: Vec<Expr>,
    pub sort_by: Vec<Expr>,
    pub having: Option<Expr>,
}

//////////////////////////////
pub struct LateralView {
    pub laterval_view: Expr,
    pub laterval_view_name: ObjectName,
    pub laterval_col_alias: Vec<Ident>,
    pub outer: bool
}

impl fmt::Display for LateralView {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            " LATERAL VIEW{outer} {} {}",
            self.laterval_view,
            self.laterval_view_name,
            // write! macro lexical, match LATERAL VIEW{outer} {} {} => outer
            outer = if self.outer { " OUTER" } else { "" })?;
        if !self.lateral_col_alias.is_empty() {
            write!(f,
                   " AS {}",
                   display_comma_separated(&self.laterval_col_alias))?;
        }

        Ok(())
    }
}

//////////////////////////////
pub struct With {
    pub recursive: bool,
    pub cte_tables: Vec<Cte>
}

impl fmt::Display for With {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "WITH {}{}",
               if self.recursive { "RECURSIVE" } else { "" },
               display_comma_separated(&self.cte_tables)
        )
    }
}

//////////////////////////////
pub struct Cte {
    pub alias: TableAlias,
    pub query: Query,
    pub from: Option<Ident>
}

impl fmt::Display for Cte {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} AS ({})", self.alias, self.query)?;
        if let Some(ref fr) = self.from {
            write!(f, " FROM {}", fr)?;
        };

        Ok(())
    }
}

//////////////////////////////
pub struct TableAlias {
    pub name: Ident,
    pub columns: Vec<Ident>
}

impl fmt::Display for TableAlias {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if !self.columns.is_empty() {
            write!(f, " ({})", display_comma_separated(&self.columns))?;
        }

        Ok(())
    }
}
