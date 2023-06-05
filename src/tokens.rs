use logos::Logos;

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
    #[token("do")]
    Do,



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

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,

    #[token("putchar")]
    Putchar,

    #[token("dup")]
    Dup,
}