use super::{DataType, Ident, ObjectName};

pub struct ColumnDef {
    pub name: Ident,
    pub data_type: DataType,
    pub object_name: ObjectName
}
