use std::vec;

use codespan::{self};
use inkwell::values::AnyValue;

use crate::complier::Compiler;

pub type Span = codespan::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub items: Vec<Item>,
    pub span: Span,
}

impl File {
    pub fn new(items: Vec<Item>, span: Span) -> File {
        File { items, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Extern(FunctionDecl),
    Function(Function),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unaryop {
    UAdd,
    USub,
    Invert,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub ident: Ident,
    pub args: Vec<Ident>,
    pub span: Span,
}

impl FunctionDecl {
    pub fn new(ident: Ident, args: Vec<Ident>, span: Span) -> FunctionDecl {
        FunctionDecl { ident, args, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub decl: FunctionDecl,
    pub body: Option<Expr>,
    pub span: Span,
    pub is_anon: bool,
}

impl Function {
    pub fn new(decl: FunctionDecl, body: Option<Expr>, span: Span) -> Function {
        Function {
            decl,
            body,
            span,
            is_anon: false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(Ident),
    Literal(Literal),
    FunctionCall(FunctionCall),
    Assign {
        ident: Ident,
        value: Box<Expr>,
    },
    BinOp {
        left: Box<Expr>,
        op: Operator,
        right: Box<Expr>,
    },
    UnaryOp {
        operand: Box<Expr>,
        op: Unaryop,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub span: Span,
}

impl Ident {
    pub fn new<S: Into<String>>(name: S, span: Span) -> Ident {
        Ident {
            name: name.into(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub value: f64,
    pub span: Span,
}

impl Literal {
    pub fn new(value: f64, span: Span) -> Literal {
        Literal { value, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub ident: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

impl FunctionCall {
    pub fn new(ident: Ident, args: Vec<Expr>, span: Span) -> FunctionCall {
        FunctionCall { ident, args, span }
    }
}

impl File {
    pub fn print_to_string(&self, complier: &Compiler) {
        for ele in self.items.clone() {
            match ele {
                Item::Extern(e) => {
                    let decl = complier.compile_fn_decl(&e).unwrap();
                    println!("{}", decl.print_to_string().to_string())
                }
                Item::Function(f) => {
                    let func = complier.compile_fn(&f).unwrap();
                    println!("{}", func.print_to_string().to_string())
                }
                Item::Expr(e) => {
                    let func = Function {
                        decl: FunctionDecl {
                            ident: Ident {
                                name: "top".to_owned(),
                                span: Span::initial(),
                            },
                            args: vec![],
                            span: Span::initial(),
                        },
                        body: Some(e),
                        span: Span::initial(),
                        is_anon: true,
                    };
                    let expr = complier.compile_fn(&func).unwrap();
                    println!("{}", expr.print_to_string().to_string())
                }
            }
        }
    }
}
