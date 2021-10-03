use std::fmt;

pub enum UnaryOperator {
    Plus,
    Minus,
    Not
}

impl fmt::Display for UnaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::Not => "NOT",
        })
    }
}

/////////////////////////////
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulus,
    StringConcat,
    Gt,
    Lt,
    GtEq,
    LtEq,
    Spaceship,
    Eq,
    NotEq,
    And,
    Or,
    Like,
    NotLike,
    ILike,
    NotILike,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
}

impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinaryOperator::*;

        f.write_str(match self {
            Plus => "+",
            Minus => "-",
            Multiply => "*",
            Divide => "/",
            Modulus => "%",
            StringConcat => "||",
            Gt => ">",
            Lt => "<",
            GtEq => ">=",
            LtEq => "<=",
            Spaceship => "<=>",
            Eq => "=",
            NotEq => "<>",
            And => "AND",
            Or => "OR",
            Like => "LIKE",
            NotLike => "NOT LIKE",
            ILike => "ILIKE",
            NotILike => "NOT ILIKE",
            BitwiseOr => "|",
            BitwiseAnd => "&",
            BitwiseXor => "^",
        })
    }
}
