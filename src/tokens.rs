use logos::Logos;

#[allow(dead_code)]
pub enum LexicalError {
  InvalidToken { tok: char },
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(skip r"//.*\n?")]
pub enum Token {
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,

    #[regex(r"\d+", |lex| lex.slice().parse::<u64>().unwrap())]
    Integer(u64),

    #[token("{")]
    LCurlyBracket,
    #[token("}")]
    RCurlyBracket,

    #[token("==")]
    Eq,
    #[token(">=")]
    Gte,
    #[token("<=")]
    Lte,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,

    #[token("or")]
    Or,

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,

    #[token("putchar")]
    PutChar,
    #[token("putint")]
    PutInt,
    #[token("dbg")]
    Dbg,

    #[token("dup")]
    Dup,
    #[token("swap")]
    Swap,
    #[token("over")]
    Over,
    #[token("rot")]
    Rot,

    #[token("drop")]
    Drop,

    #[token("mem")]
    Mem,
    #[token("store")]
    Store,
    #[token("load")]
    Load,

    #[regex(r":\w+", |lex| lex.slice()[1..].to_string())]
    FnName(String),
}