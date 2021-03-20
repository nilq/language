use logos::Span;

use crate::{compiler::State, lexer::token::Token};

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
    Eq,
    NEq,
    LEq,
    GEq,
    And,
    Or,
    Nor,
    Lt,
    Gt,
}

impl Op {
    pub const fn from(s: &Token) -> Option<(Self, u8)> {
        use self::Op::*; 

        let op = match s {
            Token::Add  => Add,
            Token::Sub  => Sub,
            Token::Mul  => Mul,
            Token::Div  => Div,
            Token::Mod  => Mod,
            Token::Pow  => Pow,
            Token::Eq   => Eq,
            Token::NEq  => NEq,
            Token::LEq  => LEq,
            Token::GEq  => GEq,
            Token::And  => And,
            Token::Or   => Or,
            Token::Nor  => Nor,
            Token::Lt   => Lt,
            Token::Gt   => Gt,

            _ => return None
        };

        let prec = op.prec();

        Some((op, prec))
    }

    pub const fn prec(&self) -> u8 {
        use self::Op::*;

        match *self {
            Eq   => 0,
            NEq  => 0,
            LEq  => 0,
            GEq  => 0,
            And  => 0,
            Or   => 0,
            Nor  => 0,
            Lt   => 0,
            Gt   => 0,
            Add  => 1,
            Sub  => 1,
            Mul  => 2,
            Div  => 2,
            Mod  => 2,
            Pow  => 3,
        }
    } 
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    Var(Binding),
    Number(f64),
    String(String),
    Binary(Box<Expr>, Op, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Not(Box<Expr>),
    Neg(Box<Expr>),
    List(Vec<Expr>),
    Map(Vec<(Expr, Expr)>),
    Index(Box<Expr>, Box<Expr>),
    True,
    False,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub node: ExprNode,
    pub span: Span
}

impl Expr {
    pub fn new(node: ExprNode, span: Span) -> Self {
        Expr {
            node,
            span
        }
    }
}

#[derive(Debug, Clone)]
pub struct Binding {
    pub name: String,
    pub depth: Option<usize>,
    pub function_depth: usize,
}

impl Binding {
    pub fn from(name: String, binding: &Binding) -> Self {
        Binding {
            name,
            depth: binding.depth,
            function_depth: binding.function_depth
        }
    }

    pub fn local(name: String, depth: usize, function_depth: usize) -> Self {
        Binding {
            name: name,
            depth: Some(depth),
            function_depth: function_depth
        }
    }

    pub fn global(name: String) -> Self {
        Binding {
            name: name,
            depth: None,
            function_depth: 0
        }
    }

    pub fn resolve(&mut self, depth: usize, function_depth: usize) {
        self.depth = Some(depth);
        self.function_depth = function_depth
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
        self.depth
            .map(|d| d > self.function_depth)
            .unwrap_or(false)
    }

    pub fn upvalue_depth(&self) -> Option<usize> {
        self.depth.and_then(|d|
            if self.is_upvalue() {
                Some(d - self.function_depth)
            } else {
                None
            })
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

type Branch = (Vec<Expr>, Statement, Binding, usize);

#[derive(Debug, Clone)]
pub enum StatementNode {
    Assign(Binding, Expr),
    SetElement(Expr, Expr, Expr),
    Break,
    Fn(Binding, Box<Branch>),
    FnCluster(Vec<Statement>),
    Global(Binding, Expr),
    Let(Binding, Expr),
    Block(Vec<Statement>),
    Expr(Expr),
    Print(Expr),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    Return(Expr),
    Label(String),
    Goto(String),
    Repeat(usize, Box<Statement>),
    While(Expr, Box<Statement>)
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub node: StatementNode,
    pub span: Span
}

impl Statement {
    pub fn new(node: StatementNode, span: Span) -> Self {
        Statement {
            node,
            span
        }
    }
}