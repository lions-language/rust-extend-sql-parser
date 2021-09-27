use crate::ast::*;

pub struct Query {
    pub with: Option<With>,
    pub body: SetExpr,
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
    Values(Values),
    Insert(Statement)
}

//////////////////////////////
pub enum SetOperator {
    Union,
    Except,
    Intersect,
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
}
