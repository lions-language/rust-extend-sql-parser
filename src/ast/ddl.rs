use super::{display_comma_separated, DataType, Ident, ObjectName, Expr};
use std::fmt;

/////////////////////////////
pub enum AlterTableOperation {
    AddConstraint(TableConstraint),
    AddColumn {
        column_def: ColumnDef
    },
    DropConstraint {
        name: Ident
    },
    DropColumn {
        column_name: Ident,
        if_exists: bool,
        caseade: bool
    },
    RenamePartitions {
        old_partitions: Vec<Expr>,
        new_partitions: Vec<Expr>
    },
    AddPartitions {
        if_not_exists: bool,
        new_partitions: Vec<Expr>,
    },
    DropPartitions {
        partitions: Vec<Expr>,
        if_exists: bool,
    },
    RenameColumn {
        old_column_name: Ident,
        new_column_name: Ident,
    },
    RenameTable {
        table_name: ObjectName
    }
}

impl fmt::Display for AlterTableOperation {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use AlterTableOperation::*;

        match self {
            AddPartitions {
                if_not_exists,
                new_partitions
            } => {
            }
        }
    }
}

/////////////////////////////
pub enum TableConstraint {
    Unique {
        name: Option<Ident>,
        columns: Vec<Ident>,
        is_primary: bool,
    },
    ForeignKey {
        name: Option<Ident>,
        columns: Vec<Ident>,
        foreign_table: ObjectName,
        referred_columns: Vec<Ident>,
    },
    Check {
        name: Option<Ident>,
        expr: Box<Expr>
    }
}

// column option
pub enum ColumnOption {
    // null
    Null,
    // not null
    NotNull,
    // default expr
    Default(Expr),
    Unique { is_primary: bool }
}

impl fmt::Display for ColumnOption {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ColumnOption::*;
        match self {
            Null => {
                write!(f, "NULL")
            },
            NotNull => {
                write!(f, "NOT NULL")
            },
            Default(expr) => {
                write!(f, "DEFAULT {}", expr)
            },
            Unique { is_primary } => {
                write!(f, "{}", if *is_primary { "PRIMARY KEY" } else { "UNIQUE" })
            }
        }
    }
}

/////////////////////////////
pub struct ColumnDef {
    pub name: Ident,
    pub data_type: DataType,
    pub collation: Option<ObjectName>,
    pub options: Vec<ColumnOptionDef>
}

impl fmt::Display for ColumnDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {}", self.name, self.data_type)?;
        for option in &self.options {
            write!(f, " {}", option)?;
        }
        Ok(())
    }
}

/////////////////////////////
fn display_constraint_name(name: &'_ Option<Ident>) -> impl fmt::Display + '_ {
    struct ConstraintName<'a>(&'a Option<Ident>);
    
    impl<'a> fmt::Display for ConstraintName<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            if let Some(n) = self.0 {
                write!(f, "CONSTRAINT {} ", n)?;
            };
            Ok(())
        }
    }

    ConstraintName(name)
}

/////////////////////////////
pub struct ColumnOptionDef {
    pub name: Option<Ident>,
    pub option: ColumnOption
}

impl fmt::Display for ColumnOptionDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", display_constraint_name(&self.name), self.option)
    }
}
