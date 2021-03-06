use logos::{Logos, Lexer};

fn mio(lex: &mut Lexer<Token>) -> Option<f64> {
    let slice = lex.slice();
    let n: f64 = slice[..slice.len() - 1].parse().ok()?;

    Some(n * 1_000_000.0)
}

fn mia(lex: &mut Lexer<Token>) -> Option<f64> {
    let slice = lex.slice();
    let n: f64 = slice[..slice.len() - 1].parse().ok()?;

    Some(n * 1_000_000_000.0)
}

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    #[token("func")]
    Match,
    #[token("|")]
    Pipe,
    #[token("global")]
    Global,
    #[token("print")]
    Print,
    #[token("let")]
    Let,
    #[token("goto")]
    Goto,
    #[token("free")]
    Free,
    #[token("->")]
    Arrow,
    #[token("=>")]
    ThiccArrow,
    #[token("@")]
    At,
    #[regex(r#""(\\.|[^"\\])*""#, |lex| lex.slice()[1..lex.slice().len() - 1].to_owned())]
    String(String),

    #[regex("[a-zA-ZæøåÆØÅ?!_]+", |lex| lex.slice().to_owned())]
    Name(String),
    #[token("=")]
    Assign,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,

    #[regex(r"[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)+m", mio)]
    #[regex(r"[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)+b", mia)]
    #[regex(r"[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)", |lex| lex.slice().parse())]
    Number(f64),

    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("^")]
    Pow,
    #[token("%")]
    Mod,

    #[token("==")]
    Eq,
    #[token("!=")]
    NEq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    LEq,
    #[token(">=")]
    GEq,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("nor")]
    Nor,

    #[token("not")]
    Not,
    #[token("break")]
    Break,

    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("True")]
    PythonTrue,
    #[token("False")]
    PythonFalse,

    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,

    #[token("if")]
    If,
    #[token("unless")]
    Unless,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,

    #[token("repeat")]
    Repeat,
    #[token("while")]
    While,

    #[token("return")]
    Return,

    // #[regex(r"[\t]+", |lex| lex.slice().len() as u8)]
    // Indent(u8),
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"(#.*)", logos::skip)]
    Error,
}