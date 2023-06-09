#[derive(Debug, PartialEq, Clone)]
pub struct Located<T> {
    pub location: (usize, usize),
    pub node: T,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceUnit {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockKind {
    pub exprs: Vec<Expr>,
}

pub type Block = Located<BlockKind>;

#[derive(Debug, PartialEq, Clone)]
pub struct Macro {
    pub name: String,
    pub args: Vec<String>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
pub enum DeclarationKind {
    Function {
        name: String,
        body: Block,
    },
    Macro(Macro),
}

pub type Declaration = Located<DeclarationKind>;

#[derive(Debug, PartialEq, Clone)]
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

    Drop,

    Mem,
    Store,
    Load,

    Conditional {
        then_block: Block,
        maybe_else_block: Option<Block>,
    },

    While {
        while_exprs: Vec<Expr>,
        do_block: Block,
    },

    FnCall(String),

    MacroCall {
        name: String,
        args: Vec<Expr>,
    },

    Block(BlockKind),

    Binding(String),
}

pub type Expr = Located<ExprKind>;
