use logos::Span;

use super::{ast::{Statement, StatementNode, ExprNode, Expr, Binding, Op}, symtab};
use super::symtab::SymTab;

use crate::{lexer::token::Token};

type TokenInfo = (Token, Span);

pub struct Parser {
    pub stack: Vec<TokenInfo>,
    pub ast:   Vec<Statement>,

    symtab: SymTab,
}

impl Parser {
    pub fn new(stack: Vec<TokenInfo>) -> Self {
        Parser {
            stack,
            ast: Vec::new(),

            symtab: SymTab::new(),
        }
    }

    pub fn parse(mut self) -> Result<Vec<Statement>, ()> {
        while self.remaining() > 0 {
            let s = self.statement()?;
            self.ast.push(s);
        }

        Ok(self.ast)
    }

    fn statement(&mut self) -> Result<Statement, ()> {
        use self::Token::*;

        let current = self.next();

        let s = match current.0 {
            Let => {
                if let Name(name) = self.next().0 {
                    self.eat(Assign)?;

                    let right = self.expression()?;

                    Statement::new(
                        StatementNode::Let(self.symtab.assign_local(name), right),
                        current.1
                    )

                } else {
                    return Err(())
                }
            },

            Global => {
                let current = self.next();
                if let Name(name) = current.0 {
                    self.eat(Assign)?;

                    let right = self.expression()?;

                    Statement::new(
                        StatementNode::Global(self.symtab.assign_global(name), right),
                        current.1
                    )

                } else {
                    if current.0 == Match {
                        if let Name(name) = self.next().0 {
                            self.eat(Arrow)?;

                            let body = self.patterns()?;

                            return Ok(
                                Statement::new(
                                StatementNode::Fn(self.symtab.assign_global(name), body),
                                current.1
                                )
                            )

                        } else {
                            return Err(())
                        }
                    }

                    return Err(())
                }
            },

            Print => Statement::new(
                StatementNode::Print(self.expression()?),
                current.1
            ),

            LBrace => {
                let mut body = Vec::new();
                loop {
                    if self.remaining() > 0 {
                        if self.top().0 == RBrace {
                            self.next();
                            break
                        }

                        body.push(self.statement()?)
                    } else {
                        return Err(())
                    }
                }

                Statement::new(
                    StatementNode::Block(body),
                    current.1
                )
            }

            Match => {
                if let Name(name) = self.next().0 {
                    self.eat(Arrow)?;
                    self.symtab.assign_local(name.clone());

                    let body = self.patterns()?;
                    let mut funcs = Vec::new();

                    for pattern in body {
                        let name = Self::mangle_name(name.clone(), &pattern.0);

                        let func = Statement::new(
                            StatementNode::Fn(self.symtab.assign_local(name), vec!(pattern)),
                            current.1.clone()
                        );

                        funcs.push(func)
                    }


                    Statement::new(
                        StatementNode::Block(funcs),
                        current.1
                    )

                } else {
                    return Err(())
                }
            }

            _ => {
                let span = current.1.clone();
                self.stack.insert(0, current);

                Statement::new(
                    StatementNode::Expr(self.expression()?),
                    span
                )
            }
        };

        Ok(s)
    }

    fn patterns(&mut self) -> Result<Vec<(Vec<Expr>, Statement)>, ()> {
        let mut body = Vec::new();
        self.eat(Token::LBrace)?;
        loop {
            if self.remaining() > 0 {
                if self.top().0 == Token::RBrace {
                    self.next();
                    break
                }

                body.push(self.pattern()?)
            } else {
                return Err(())
            }
        }

        Ok(body)
    }

    fn pattern(&mut self) -> Result<(Vec<Expr>, Statement), ()> {
        self.eat(Token::Pipe)?;
        self.symtab.enter_func();

        let mut params = Vec::new();
        loop {
            if self.remaining() > 0 {
                if self.top().0 == Token::ThiccArrow {
                    self.next();
                    break
                }

                if let (Token::Name(n), span) = self.top() {
                    self.symtab.assign_local(n.clone());
                }

                let expr = self.expression()?;

                params.push(expr)
            } else {
                return Err(())
            }
        }

        let body = self.statement()?;

        self.symtab.yeet();

        Ok((params, body))
    }

    fn expression(&mut self) -> Result<Expr, ()> {
        let mut atom = self.atom()?;

        if self.remaining() > 0 {
            atom = self.postfix(atom)?;
        }

        if self.remaining() > 0 && Self::is_operator(&self.top().0) {
            self.parse_binary(atom)
        } else {
            Ok(atom)
        }
    }

    fn postfix(&mut self, expr: Expr) -> Result<Expr, ()> {
        use self::Token::*;

        let current =  self.top().clone();

        let e = match current.0 {
            LParen => {
                self.next();
                let mut args = Vec::new();

                loop {
                    if self.remaining() > 0 {
                        if self.top().0 == Token::RParen {
                            self.next();
                            break
                        }

                        let expr = self.expression()?;

                        args.push(expr)
                    } else {
                        return Err(())
                    }
                }

                let mut expr = expr;

                if let ExprNode::Var(ref mut binding) = expr.node {
                    let mut name = binding.name.clone();

                    let mangle_name = Self::mangle_name(binding.name.clone(), &args);

                    let mut cache = Vec::new();
                    let mut found = false;

                    'outer: for scope in self.symtab.scopes.iter() {
                        for compatible_name in scope.variables
                                .iter()
                                .filter(|&(x, _)| x.starts_with(&binding.name)) {

                            if compatible_name.0 == &mangle_name {
                                name = mangle_name;
                                found = true;

                                break 'outer
                            }

                            cache.push(&scope.variables[compatible_name.0])
                        }
                    }

                    if !found {
                        let mask_name = Self::mangle_mask_name(binding.name.clone(), &args);
                        let mask = Self::name_mask(&args);

                        found = false;

                        for binding in cache.iter() {
                            if !binding.name.contains("$") {
                                continue
                            }

                            let binding_mask = &binding.name.split("$").collect::<Vec<&str>>()[1..][..1];

                            if mask == binding_mask {
                                name = binding.name.clone();
                                found = true;
                                break
                            }
                        }

                        if !found {
                            for binding in cache.iter() {
                                if !binding.name.contains("$") {
                                    continue
                                }

                                let binding_mask = &binding.name.split("$").collect::<Vec<&str>>()[1..][..1];
                                let bool_mask = mask.iter()
                                    .enumerate()
                                    .map(|(i, x)| i < binding_mask.len() && x == binding_mask[i])
                                    .collect::<Vec<bool>>();
    
                                let mut inner_found = true;

                                if !bool_mask.contains(&true) {
                                    continue
                                }
    
                                for (i, mask) in mask.iter().enumerate() {
                                    if !bool_mask[i] && i < binding_mask.len() && binding_mask[i] != "v" {
                                        inner_found = false;
                                        break
                                    }
                                }
    
                                if inner_found {
                                    found = true;
                                    name = binding.name.clone();
                                    break
                                }
                            }

                            // Still not.
                            if !found {
                                for binding in cache {
                                    if mask_name == binding.name {
                                        found = true;
                                        name = binding.name.clone();
                                        break
                                    }
                                }
                            }
                        }
                    }

                    binding.name = name;

                    Expr::new(
                        ExprNode::Call(Box::new(expr), args),
                        current.1
                    )
                } else {
                    Expr::new(
                        ExprNode::Call(Box::new(expr), args),
                        current.1
                    )
                }
            },
            _ => expr
        };

        Ok(e)
    }

    fn atom(&mut self) -> Result<Expr, ()> {
        use self::Token::*;

        let e = match self.next() {
            (Number(n), span) => Expr::new(
                ExprNode::Number(n),
                span
            ),

            (String(s), span) => Expr::new(
                ExprNode::String(s),
                span
            ),

            (Name(name), span) => if let Some(binding) =  self.symtab.get(&name) {
                Expr::new(
                    ExprNode::Var(
                        binding.clone()
                    ),
                    span
                )
            } else {
                println!("Can't find {}", name);
                return Err(())
            }

            c => todo!("{:?}", c)
        };

        Ok(e)
    }

    fn parse_binary(&mut self, left: Expr) -> Result<Expr, ()> {
        let left_position = left.span.clone();

        let mut expression_stack = vec![left];
        let mut operator_stack = vec![Op::from(&self.next().0).unwrap()];

        expression_stack.push(self.atom()?);

        while operator_stack.len() > 0 {
            while self.remaining() > 0 && Self::is_operator(&self.top().0) {
                let (operator, precedence) = Op::from(&self.next().0).unwrap();

                if precedence < operator_stack.last().unwrap().1 {
                    let right = expression_stack.pop().unwrap();
                    let left = expression_stack.pop().unwrap();

                    expression_stack.push(Expr::new(
                        ExprNode::Binary(
                            Box::new(left),
                            operator_stack.pop().unwrap().0,
                            Box::new(right),
                        ),
                        self.top().1.clone(),
                    ));

                    if self.remaining() > 0 {
                        expression_stack.push(self.atom()?);
                        operator_stack.push((operator, precedence))
                    } else {
                        return Err(())
                    }
                } else {
                    expression_stack.push(self.atom()?);
                    operator_stack.push((operator, precedence))
                }
            }

            let right = expression_stack.pop().unwrap();
            let left = expression_stack.pop().unwrap();

            expression_stack.push(Expr::new(
                ExprNode::Binary(
                    Box::new(left),
                    operator_stack.pop().unwrap().0,
                    Box::new(right),
                ),
                left_position.clone(),
            ));
        }

        let expression = expression_stack.pop().unwrap();

        Ok(Expr::new(
            expression.node,
            left_position,
        ))
    }

    fn eat(&mut self, token: Token) -> Result<TokenInfo, ()> {
        self.expect(token)?;
        Ok(self.next())
    }

    fn expect(&self, token: Token) -> Result<(), ()> {
        if self.remaining() > 0 {
            if self.top().0 != token {
                return Err(())
            }
        }

        Ok(())
    }

    fn is_operator(token: &Token) -> bool {
        use self::Token::*;
        [Add, Sub, Mul, Div, Pow, Mod].contains(token)
    }

    fn top(&self) -> &TokenInfo {
        &self.stack[0]
    }

    fn next(&mut self) -> TokenInfo {
        self.stack.remove(0)
    }

    fn remaining(&self) -> usize {
        self.stack.len()
    }

    fn mangle_name(name: String, exes: &Vec<Expr>) -> String {
        let mut name = name;
        let mut arity = 0;

        use self::ExprNode::*;

        for expr in exes.iter() {
            match expr.node {
                Var(_) => {
                    arity += 1;
                    name.push_str("$v")
                },
                Number(n) => {
                    name.push_str("$");
                    name.push_str(&n.to_string())
                }
                String(_) => {
                    name.push_str("$str")
                }
                _ => ()
            }
        }

        name.push_str("__");
        name.push_str(&arity.to_string());

        name
    }

    fn mangle_mask_name(name: String, exes: &Vec<Expr>) -> String {
        let mut name = name;

        for expr in exes.iter() {
            name.push_str("$v")
        }

        name.push_str("__");
        name.push_str(&exes.len().to_string());

        name
    }

    fn name_mask(exes: &Vec<Expr>) -> Vec<String> {
        let mut mask = Vec::new();

        use self::ExprNode::*;

        for expr in exes.iter() {
            match expr.node {
                Var(_) => {
                    mask.push("v".to_string())
                },
                Number(n) => {
                    mask.push(n.to_string())
                }
                String(_) => {
                    mask.push("s".to_string())
                }
                _ => ()
            }
        }

        mask
    }
}