use logos::Logos;

#[derive(Logos, Copy, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'input> {
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice())]
    Identifier(&'input str),
    #[regex("[-]?[0-9]*[.]?[0-9]+(?:[eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    Number(f64),
    #[token("def")]
    Def,
    #[token("extern")]
    Extern,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("end")]
    End,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("=")]
    Assign,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,
    #[token("~")]
    Invert,
    #[token(">")]
    More,
    #[token("<")]
    Less,
    #[token(">=")]
    MoreEq,
    #[token("<=")]
    LessEq,
    #[token("==")]
    Equal,
}

impl<'input> Token<'input> {
    pub fn as_ident(&self) -> Option<&'input str> {
        match *self {
            Token::Identifier(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<f64> {
        match *self {
            Token::Number(n) => Some(n),
            _ => None,
        }
    }
}

impl<'a> From<i32> for Token<'a> {
    fn from(other: i32) -> Token<'a> {
        Token::Number(other as f64)
    }
}

impl<'a> From<f64> for Token<'a> {
    fn from(other: f64) -> Token<'a> {
        Token::Number(other)
    }
}

impl<'a> From<&'a str> for Token<'a> {
    fn from(other: &'a str) -> Token<'a> {
        Token::Identifier(other)
    }
}
