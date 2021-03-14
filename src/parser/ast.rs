use logos::Span;

use crate::lexer::token::Token;

#[derive(Debug, Clone)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Mod,
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

            _ => return None
        };

        let prec = op.prec();

        Some((op, prec))
    }

    pub const fn prec(&self) -> u8 {
        use self::Op::*;

        match *self {
            Add  => 0,
            Sub  => 0,
            Mul  => 1,
            Div  => 1,
            Mod  => 1,
            Pow  => 2,
        }
    } 
}

#[derive(Debug, Clone)]
pub enum ExprNode {
    Var(Binding),
    Number(f64),
    String(String),
    Binary(Box<Expr>, Op, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>)
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

type Branch = (Vec<Expr>, Statement);

#[derive(Debug, Clone)]
pub enum StatementNode {
    Assign(Binding, Expr),
    Fn(Binding, Vec<Branch>),
    Global(Binding, Expr),
    Let(Binding, Expr),
    Block(Vec<Statement>),
    Expr(Expr),
    Print(Expr),
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