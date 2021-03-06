use crate::ast::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OrderByExpr {
    pub expr: Expr,
    pub asc: Option<bool>,
    pub nulls_first: Option<bool>
}

impl fmt::Display for OrderByExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.expr)?;
        match self.asc {
            Some(true) => write!(f, " ASC")?,
            Some(false) => write!(f, " DESC")?,
            None => (),
        }
        match self.nulls_first {
            Some(true) => write!(f, " NULLS FIRST")?,
            Some(false) => write!(f, " NULLS LAST")?,
            None => (),
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum OffsetRows {
    None,
    Row,
    Rows,
}

impl fmt::Display for OffsetRows {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OffsetRows::None => Ok(()),
            OffsetRows::Row => write!(f, " ROW"),
            OffsetRows::Rows => write!(f, " ROWS"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Offset {
    pub value: Expr,
    pub rows: OffsetRows,
}

impl fmt::Display for Offset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "OFFSET {} {}", self.value, self.rows)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Fetch {
    pub with_ties: bool,
    pub percent: bool,
    pub quantity: Option<Expr>
}

impl fmt::Display for Fetch {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let extension = if self.with_ties { "WITH TIES" } else { "ONLY" };
        if let Some(ref quantity) = self.quantity {
            let percent = if self.percent { " PERCENT" } else { "" };
            write!(f, "FETCH FIRST {}{} ROWS {}", quantity, percent, extension)
        } else {
            write!(f, "FETCH FIRST ROWS {}", extension)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Query {
    pub with: Option<With>,
    pub body: SetExpr,
    pub order_by: Vec<OrderByExpr>,
    pub limit: Option<Expr>,
    pub offset: Option<Offset>,
    pub fetch: Option<Fetch>
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Values(pub Vec<Vec<Expr>>);

impl fmt::Display for Values {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "VALUES ")?;
        let mut delim = "";
        for row in &self.0 {
            write!(f, "{}", delim)?;
            delim = ", ";
            write!(f, "({})", display_comma_separated(row))?;
        }

        Ok(())
    }
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SetExpr {
    Select(Box<Select>),
    Query(Box<Query>),
    SetOperation {
        op: SetOperator,
        all: bool,
        left: Box<SetExpr>,
        right: Box<SetExpr>,
    },
    Values(Values),
    Insert(Statement)
}

impl fmt::Display for SetExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SetExpr::Select(s) => write!(f, "{}", s)?,
            SetExpr::Query(q) => write!(f, "{}", q)?,
            SetExpr::Values(v) => write!(f, "{}", v)?,
            SetExpr::Insert(v) => write!(f, "{}", v)?,
            SetExpr::SetOperation {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SetOperator {
    Union,
    Except,
    Intersect,
}

impl fmt::Display for SetOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            SetOperator::Union => "UNION",
            SetOperator::Except => "EXCEPT",
            SetOperator::Intersect => "INTERSECT",
        })
    }
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Top {
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SelectItem {
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
    pub distribute_by: Vec<Expr>,
    pub sort_by: Vec<Expr>,
    pub having: Option<Expr>,
}

impl fmt::Display for Select {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "SELECT{}", if self.distinct { " DISTINCT" } else { "" })?;
        if let Some(ref top) = self.top {
            write!(f, " {}", top)?;
        }
        write!(f, " {}", display_comma_separated(&self.projection))?;
        if !self.from.is_empty() {
            write!(f, " FROM {}", display_comma_separated(&self.from))?;
        }
        if !self.lateral_views.is_empty() {
            for lv in &self.lateral_views {
                write!(f, "{}", lv)?;
            }
        }
        if let Some(ref selection) = self.selection {
            write!(f, " WHERE {}", selection)?;
        }
        if !self.group_by.is_empty() {
            write!(f, " GROUP BY {}", display_comma_separated(&self.group_by))?;
        }
        if !self.cluster_by.is_empty() {
            write!(f,
                   "CLUSTER BY {}",
                   display_comma_separated(&self.cluster_by)
            )?;
        }
        if !self.distribute_by.is_empty() {
            write!(f,
                   " DISTRIBUTE BY {}",
                   display_comma_separated(&self.distribute_by)
            )?;
        }
        if !self.sort_by.is_empty() {
            write!(f, " SORT BY {}", display_comma_separated(&self.sort_by))?;
        }
        if let Some(ref having) = self.having {
            write!(f, " HAVING {}", having)?;
        }

        Ok(())
    }
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
        if !self.laterval_col_alias.is_empty() {
            write!(f,
                   " AS {}",
                   display_comma_separated(&self.laterval_col_alias))?;
        }

        Ok(())
    }
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum JoinConstraint {
    On(Expr),
    Using(Vec<Ident>),
    Natural,
    None
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum JoinOperator {
    Inner(JoinConstraint),
    LeftOuter(JoinConstraint),
    RightOuter(JoinConstraint),
    FullOuter(JoinConstraint),
    CrossJoin,
    CrossApply,
    OuterApply
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Join {
    pub relation: TableFactor,
    pub join_operator: JoinOperator,
}

impl fmt::Display for Join {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use JoinConstraint::*;

        // inner function
        fn prefix(constraint: &JoinConstraint) -> &'static str {
            match constraint {
                Natural => "NATURAL ",
                _ => "",
            }
        }

        fn suffix(constraint: &JoinConstraint) -> impl fmt::Display + '_ {
            struct Suffix<'a>(&'a JoinConstraint);
            impl<'a> fmt::Display for Suffix<'a> {
                fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                    match self.0 {
                        JoinConstraint::On(expr) => write!(f, " ON {}", expr)?,
                        JoinConstraint::Using(attrs) => {
                            write!(f, " USING({})", display_comma_separated(attrs))?;
                        },
                        _ => {
                            return Ok(());
                        }
                    }

                    Ok(())
                }
            }

            Suffix(constraint)
        }

        use JoinOperator::*;
        match &self.join_operator {
            Inner(constraint) => {
                write!(f,
                       " {}JOIN {}{}",
                       prefix(constraint),
                       self.relation,
                       suffix(constraint)
                )?;
            },
            LeftOuter(constraint) => {
                write!(f,
                       " {}LEFT JOIN {}{}",
                       prefix(constraint),
                       self.relation,
                       suffix(constraint)
                )?;
            },
            RightOuter(constraint) => {
                write!(f,
                       " {}RIGHT JOIN {}{}",
                       prefix(constraint),
                       self.relation,
                       suffix(constraint)
                )?;
            },
            FullOuter(constraint) => {
                write!(f,
                       " {}FULL JOIN {}{}",
                       prefix(constraint),
                       self.relation,
                       suffix(constraint)
                )?;
            },
            CrossJoin => write!(f, " CROSS JOIN {}", self.relation)?,
            CrossApply => write!(f, " CROSS APPLY {}", self.relation)?,
            OuterApply => write!(f, " OUTER APPLY {}", self.relation)?
        }

        Ok(())
    }
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TableFactor {
    Table {
        name: ObjectName,
        alias: Option<TableAlias>,
        args: Vec<FunctionArg>,
        with_hints: Vec<Expr>,
    },
    Derived {
        laterval: bool,
        subquery: Box<Query>,
        alias: Option<TableAlias>,
    },
    TableFunction {
        expr: Expr,
        alias: Option<TableAlias>,
    },
    NestedJoin(Box<TableWithJoins>)
}

impl fmt::Display for TableFactor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TableFactor::*;

        match self {
            Table {
                name,
                alias,
                args,
                with_hints,
            } => {
                write!(f, "{}", name)?;
                if !args.is_empty() {
                    write!(f, "({})", display_comma_separated(args))?;
                }
                if let Some(alias) = alias {
                    write!(f, " AS {}", alias)?;
                }
                if !with_hints.is_empty() {
                    write!(f, " WITH ({})", display_comma_separated(with_hints))?;
                }
            },
            Derived {
                laterval,
                subquery,
                alias,
            } => {
                if *laterval {
                    write!(f, "LATERAL ")?;
                }
                write!(f, "({})", subquery)?;
                if let Some(alias) = alias {
                    write!(f, " AS {}", alias)?;
                }
            },
            TableFunction {
                expr,
                alias,
            } => {
                write!(f, "TABLE({})", expr)?;
                if let Some(alias) = alias {
                    write!(f, " AS {}", alias)?;
                }
            },
            NestedJoin(table_reference) => {
                write!(f, "({})", table_reference)?;
            }
        }

        Ok(())
    }
}

//////////////////////////////
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TableWithJoins {
    pub relation: TableFactor,
    pub joins: Vec<Join>
}

impl fmt::Display for TableWithJoins {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.relation)?;
        for join in &self.joins {
            write!(f, "{}", join)?;
        }

        Ok(())
    }
}
