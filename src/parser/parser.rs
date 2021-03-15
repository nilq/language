use logos::Span;
use strsim::damerau_levenshtein;

use super::{ast::{Statement, StatementNode, ExprNode, Expr, Binding, Op}, symtab};
use super::symtab::SymTab;

use crate::{error::{self, error}, lexer::token::Token};

type TokenInfo = (Token, Span);

pub struct Parser<'a> {
    pub stack: Vec<TokenInfo>,
    pub ast:   Vec<Statement>,

    symtab: SymTab,
    src: &'a String,
}

impl<'a> Parser<'a> {
    pub fn new(stack: Vec<TokenInfo>, src: &'a String) -> Self {
        Parser {
            stack,
            ast: Vec::new(),

            symtab: SymTab::new(),
            src,
        }
    }

    pub fn parse(mut self, globals: &[&str]) -> Result<Vec<Statement>, ()> {
        for global in globals {
            self.symtab.assign_global(global.to_string());
        }

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
                let next = self.next();
                if let Name(name) = next.0 {
                    self.eat(Assign)?;

                    let right = self.expression()?;

                    Statement::new(
                        StatementNode::Let(self.symtab.assign_local(name), right),
                        current.1
                    )

                } else {
                    return Err(error(&self.src, "expected a name", "found this one instead", "please add a name", next.1.clone()))
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
                        let next = self.next();
                        if let Name(name) = next.0 {
                            self.eat(Arrow)?;
                            self.symtab.assign_global(name.clone());
        
                            let body = self.patterns(&name, true)?;
                            let mut funcs = Vec::new();
        
                            for pattern in body {
                                let name = Self::mangle_name(name.clone(), &pattern.0);

                                let pure_arity = pattern.0.len();
                                let binding = self.symtab.assign_local(name);

                                let branch = (
                                    pattern.0,
                                    pattern.1,
                                    binding.clone(),
                                    pure_arity,
                                );
        
                                let func = Statement::new(
                                    StatementNode::Fn(binding, Box::new(branch)),
                                    current.1.clone()
                                );
        
                                funcs.push(func)
                            }
        
                            return Ok(
                                Statement::new(
                                StatementNode::FnCluster(funcs),
                                current.1
                                )
                            )
        
                        } else {
                            return Err(
                                error(
                                    self.src, 
                                    "expected name of function, didn't find one",
                                    "found this",
                                    "give that function a name, just a little one",
                                    next.1
                                )
                            )
                        }
                    }

                    return Err(
                        error(
                            self.src, 
                            "global what?",
                            "this can't be made global",
                            "there is global match, and then there is just global",
                            current.1
                        )
                    )
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
                        return Err(
                            error(
                                self.src, 
                                "expected `}`, but hit the end",
                                "this is the end, not a `}`",
                                "close the braces",
                                self.src.len() - 1 .. self.src.len()
                            )
                        )
                    }
                }

                Statement::new(
                    StatementNode::Block(body),
                    current.1
                )
            }

            Match => {
                let next = self.next();

                if let Name(name) = next.0 {
                    self.eat(Arrow)?;
                    self.symtab.assign_local(name.clone());

                    let body = self.patterns(&name, false)?;
                    let mut funcs = Vec::new();

                    for pattern in body {
                        let name = Self::mangle_name(name.clone(), &pattern.0);
                        let pure_arity = pattern.0.len();

                        let binding = self.symtab.assign_local(name);

                        let branch = (
                            pattern.0,
                            pattern.1,
                            binding.clone(),
                            pure_arity,
                        );

                        let func = Statement::new(
                            StatementNode::Fn(binding, Box::new(branch)),
                            current.1.clone()
                        );

                        funcs.push(func)
                    }


                    Statement::new(
                        StatementNode::FnCluster(funcs),
                        current.1
                    )

                } else {
                    return Err(
                        error(
                            self.src, 
                            "expected name of function, didn't find one",
                            "found this",
                            "give that function a name, just a little one",
                            next.1
                        )
                    )
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

    fn patterns(&mut self, name: &String, is_global: bool) -> Result<Vec<(Vec<Expr>, Statement)>, ()> {
        let mut body = Vec::new();
        self.eat(Token::LBrace)?;
        loop {
            if self.remaining() > 0 {
                if self.top().0 == Token::RBrace {
                    self.next();
                    break
                }

                body.push(self.pattern(name, is_global)?)
            } else {
                return Err(
                    error(self.src, "expected `}`, but the file just ended", "finito", "do me a favor, and close that left-brace", self.src.len() - 1..self.src.len())
                )
            }
        }

        Ok(body)
    }

    fn pattern(&mut self, name: &String, is_global: bool) -> Result<(Vec<Expr>, Statement), ()> {
        self.eat(Token::Pipe)?;

        let mut params = Vec::new();
        let mut names = Vec::new();
        loop {
            if self.remaining() > 0 {
                if self.top().0 == Token::ThiccArrow {
                    self.next();
                    break
                }

                self.symtab.enter_func();
                if let (Token::Name(n), span) = self.top() {
                    names.push(n.clone());
                    self.symtab.assign_local(n.clone());
                }

                let expr = self.expression()?;

                self.symtab.yeet();

                params.push(expr)
            } else {
                return Err(
                    error(self.src, "expected `=>`, but the file just ended?!", "what?", "finish the pattern", self.src.len() - 1..self.src.len())
                )
            }
        }

        let name = Self::mangle_name(name.clone(), &params);

        if is_global {
            self.symtab.assign_global(name);
        } else {
            self.symtab.assign_local(name);
        }

        self.symtab.enter_func();

        for name in names {
            self.symtab.assign_local(name);
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

        if self.remaining() == 0 {
            return Ok(expr)
        }

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
                        return Err(
                            error(self.src, "expected `)`, but the file just ended?!", "what?", "close the parentheses", self.src.len() - 1..self.src.len())
                        )
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
                                name = mangle_name.clone();
                                found = true;

                                break 'outer
                            }

                            cache.push(&scope.variables[compatible_name.0])
                        }
                    }

                    if !found {
                        for compatible_name in self.symtab.globals
                                .iter()
                                .filter(|&(x, _)| x.starts_with(&binding.name)) {
                            
                            if compatible_name.0 == &mangle_name {
                                name = mangle_name;
                                found = true;
                                break
                            }

                            cache.push(&self.symtab.globals[compatible_name.0])
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

                            let binding_mask = &binding.name.split("$").collect::<Vec<&str>>()
                                .iter()
                                .map(|x| &x[..1])
                                .collect::<Vec<&str>>()[1..];

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
    
                                for (i, _) in mask.iter().enumerate() {
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
            _ => return Ok(expr)
        };

        self.postfix(e)
    }

    fn atom(&mut self) -> Result<Expr, ()> {
        use self::Token::*;

        let current = self.next();

        let e = match current.0 {
            Number(n) => Expr::new(
                ExprNode::Number(n),
                current.1
            ),

            String(s) => Expr::new(
                ExprNode::String(s),
                current.1
            ),

            Name(name) => if let Some(binding) =  self.symtab.get(&name) {
                let mut binding = binding.clone();
                
                if binding.depth.is_some() {
                    binding.depth = Some(self.symtab.current_depth())
                }

                Expr::new(
                    ExprNode::Var(
                        binding
                    ),
                    current.1
                )
            } else {
                let mut similar = Vec::new();

                for scope in self.symtab.scopes.iter() {
                    for other in scope.variables.iter() {

                        if damerau_levenshtein(&name, &other.0) <= 2 {
                            similar.push(other.0.clone())
                        }
                    }
                }

                for other in self.symtab.globals.iter() {
                    if damerau_levenshtein(&name, &other.0) <= 3 {
                        similar.push(other.0.clone())
                    }
                }

                return Err(
                    error(
                        self.src, 
                        &format!("no such variable `{}`", name),
                        &format!("maybe one of these: [{}]?", similar.join(", ")),
                        "find a variable that exists",
                        current.1
                    )
                )
            }

            Not => Expr::new(
                ExprNode::Not(
                    Box::new(self.expression()?)
                ),
                current.1
            ),

            Sub => Expr::new(
                ExprNode::Neg(
                    Box::new(self.expression()?)
                ),
                current.1
            ),

            c => todo!("{:?}", c)
        };

        Ok(e)
    }

    fn parse_binary(&mut self, left: Expr) -> Result<Expr, ()> {
        let left_position = left.span.clone();

        let next = self.next();

        let mut expression_stack = vec![left];
        let mut operator_stack = vec![Op::from(&next.0).unwrap()];

        if self.remaining() > 0 {
            let atom = self.atom()?;
            expression_stack.push(self.postfix(atom)?);
        } else {
            return Err(
                error(
                    self.src, 
                    "expected operand, but did not",
                    "what are you doing here?",
                    "remember to stay hydrated, my friend",
                    next.1
                )
            )
        }

        while operator_stack.len() > 0 {
            while self.remaining() > 0 && Self::is_operator(&self.top().0) {
                let next = self.next();
                let (operator, precedence) = Op::from(&next.0).unwrap();

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
                        return Err(
                            error(
                                self.src, 
                                "expected operand, but did not",
                                "what are you doing here?",
                                "remember to stay hydrated, my friend",
                                next.1
                            )
                        )
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
            let top = self.top();
            if top.0 != token {
                return Err(
                    error(
                        self.src, 
                        &format!("expected `{:?}`-token", token),
                        "found this",
                        "it would really mean a lot if you fixed this :)",
                        top.1.clone()
                    )
                )
            }
        }

        Ok(())
    }

    fn is_operator(token: &Token) -> bool {
        use self::Token::*;
        [Add, Sub, Mul, Div, Pow, Mod, Eq, NEq, Lt, Gt, LEq, GEq, Not, And, Or, Nor].contains(token)
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
                    name.push_str("$s")
                }
                _ => {
                    arity += 1;
                    name.push_str("$v")
                }
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
                _ => {
                    mask.push("v".to_string())
                }
            }
        }

        mask
    }
}