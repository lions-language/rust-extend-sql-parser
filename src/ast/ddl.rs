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
        cascade: bool
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AlterTableOperation::*;

        match self {
            AddPartitions {
                if_not_exists,
                new_partitions
            } => {
                write!(f,
                       "ADD{ine} PARTITION ({})",
                       display_comma_separated(new_partitions),
                       ine = if *if_not_exists { " IF NOT EXISTS" } else { "" }
                )?;
            },
            AddConstraint(c) => {
                write!(f, "ADD {}", c)?;
            },
            AddColumn {
                column_def
            } => {
                write!(f, "ADD COLUMN {}", column_def.to_string())?;
            },
            DropPartitions {
                partitions,
                if_exists,
            } => {
                write!(f,
                       "DROP{ie} PARTITION ({})",
                       display_comma_separated(partitions),
                       ie = if *if_exists { " IF EXISTS"} else { "" })?;
            },
            DropConstraint {
                name
            } => {
                write!(f, "DROP CONSTRAINT {}", name)?;
            },
            DropColumn {
                column_name,
                if_exists,
                cascade,
            } => {
                write!(f,
                       "DROP COLUMN {}{}{}",
                       if *if_exists { "IF EXISTS "} else { "" },
                       column_name,
                       if *cascade { " CASCADE" } else { "" }
                )?;
            },
            RenamePartitions {
                old_partitions,
                new_partitions
            } => {
                write!(f,
                       "PARTITION ({}) RENAME TO PARTITION ({})",
                       display_comma_separated(old_partitions),
                       display_comma_separated(new_partitions)
                )?;
            },
            RenameColumn {
                old_column_name,
                new_column_name
            } => {
                write!(f,
                       "RENAME COLUMN {} TO {}",
                       old_column_name, new_column_name
                )?;
            },
            RenameTable {
                table_name
            } => {
                write!(f, "RENAME TO {}", table_name)?;
            }
        }

        Ok(())
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

impl fmt::Display for TableConstraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use TableConstraint::*;

        match self {
            Unique {
                name,
                columns,
                is_primary,
            } => {
                write!(f,
                       "{}{} ({})",
                       display_constraint_name(name),
                       if *is_primary { "PRIMARY KEY" } else { "UNIQUE" },
                       display_comma_separated(columns)
                )?;
            },
            ForeignKey {
                name,
                columns,
                foreign_table,
                referred_columns,
            } => {
                write!(f,
                       "{}FOREIGN KEY ({}) REFERENCES {}({})",
                       display_constraint_name(name),
                       display_comma_separated(columns),
                       foreign_table,
                       display_comma_separated(referred_columns)
                )?;
            },
            Check {
                name,
                expr
            } => {
                write!(f, "{}CHECK ({})", display_constraint_name(name), expr)?;
            }
        }

        Ok(())
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
