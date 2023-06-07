#[derive(Debug, PartialEq)]
pub struct Located<T> {
    pub location: (usize, usize),
    pub node: T,
}

#[derive(Debug, PartialEq)]
pub struct SourceUnit {
    pub exprs: Vec<Expr>
}

#[derive(Debug, PartialEq)]
pub enum ExprKind {
    Integer(u64),

    Eq,
    Gte,
    Lte,
    Gt,
    Lt,

    Or,

    Add,
    Sub,
    Mul,
    Div,

    Putchar,
    Putint,
    Dbg,

    Dup,
    Swap,
    Over,
    Rot,

    Pop,

    Mem,
    Store,
    Load,

    Conditional {
        then_exprs: Vec<Expr>,
        maybe_else_exprs: Option<Vec<Expr>>,
    },

    While {
        while_exprs: Vec<Expr>,
        do_exprs: Vec<Expr>,
    },
}

pub type Expr = Located<ExprKind>;
