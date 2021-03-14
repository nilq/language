use crate::vm::{
    chunk::{OpCode, Chunk},
    value::{
        value::Value,
        object::{Obj, Func},
    },
    heap::{Heap, Handle}
};
use crate::parser::ast::{Statement, StatementNode, Expr, ExprNode, Binding, Op};

use std::{ops::Deref, usize};

#[derive(Debug, Clone)]
pub struct Local {
    pub name: String,
    pub depth: usize,
    pub captured: bool,
    pub reserved: bool,
}

#[derive(Debug, Clone)]
pub struct UpValue {
    pub index: u8,
    pub is_local: bool,
}

pub struct State {
    pub locals: Vec<Local>,
    upvalues: Vec<UpValue>,

    func: Func,
    line: usize,

    scope_depth: usize,
    breaks: Vec<usize>,
}

impl State {
    pub fn new(name: String, func: Func, scope_depth: usize) -> Self {
        State {
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth,
            func,
            line: 0,
            breaks: Vec::new()
        }
    }

    fn capture_local(&mut self, var: &str) -> Option<u8> {
        for (i, local) in self.locals.iter_mut().enumerate().rev() {
            if local.name == var {
                local.captured = true;

                return Some(i as u8)
            }
        }

        None
    }

    fn add_local(&mut self, var: &str, depth: usize) -> u8 {
        let depth = self.scope_depth - depth;

        if self.locals.len() == std::u8::MAX as usize {
            panic!("[bruh moment] Local variable overflow.")
        }

        self.locals.push(
            Local {
                name: var.into(),
                depth,
                captured: false,
                reserved: false,
            }
        );

        (self.locals.len() - 1) as u8
    }

    fn resolve_local(&mut self, var: &str) -> u8 {
        for (i, local) in self.locals.iter().enumerate().rev() {
            if local.name == var {
                return i as u8
            }
        }

        todo!("Unresolved var `{}` in {:#?}", var, self.locals)
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> u8 {
        for (i, upval) in self.upvalues.iter().enumerate() {
            if upval.index == index && upval.is_local == is_local {
                return i as u8
            }
        }

        if self.upvalues.len() == std::u8::MAX as usize {
            panic!("too many upvalues, not cool")
        } else {
            self.upvalues.push(
                UpValue {
                    index,
                    is_local
                }
            );

            (self.upvalues.len() - 1) as u8
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        let last = self.scope_depth;

        self.scope_depth -= 1;

        let mut ops = Vec::new();

        self.locals.retain(|local| {
            if local.depth < last || local.reserved {
                return true
            }

            if local.captured {
                ops.push(OpCode::CloseUpValue)
            } else {
                // use backtrace::Backtrace;
                // let bt = Backtrace::new();
                ops.push(OpCode::Pop)
            }

            false
        });

        ops.into_iter().rev().for_each(|op| self.emit(op))
    }

    fn emit(&mut self, op: OpCode) {
        self.func.chunk_mut().write(op, self.line);
    }

    fn add_break(&mut self, jmp: usize) {
        self.breaks.push(jmp);
    }

    fn breaks(&mut self) -> Vec<usize> {
        let bs = self.breaks.clone();
        self.breaks.clear();

        bs
    }
}

pub struct Compiler<'a> {
    heap: &'a mut Heap<Obj>,

    pub states: Vec<State>,
    pub locals_cache: Vec<Local>
}

impl<'a> Compiler<'a> {
    pub fn new(heap: &'a mut Heap<Obj>) -> Self {
        Compiler {
            heap,
            states: Vec::new(),
            locals_cache: Vec::new(),
        }
    }

    pub fn compile(&mut self, ast: Vec<Statement>) -> Func {
        self.start_function("m3", 0, 0);

        for statement in ast.iter() {
            self.compile_statement(statement)
        }

        self.emit_return(None);
        self.end_function()
    }

    fn compile_statement(&mut self, statement: &Statement) {
        use self::StatementNode::*;

        match statement.node {
            Assign(ref binding, ref right) => {
                self.compile_expr(&right);

                if binding.is_upvalue() {
                    let i = self.resolve_upvalue(binding.name());

                    self.emit(OpCode::SetUpValue);
                    self.emit_byte(i);
                } else if binding.depth.is_none() { // Global
                    self.set_global(binding.name())
                } else {
                    let idx = self.state_mut().resolve_local(binding.name());

                    self.emit(OpCode::SetLocal);
                    self.emit_byte(idx)
                }
            },

            Let(ref binding, ref right) => {
                self.compile_expr(&right);
                self.var_define(binding, None);
            },

            Global(ref binding, ref right) => {
                self.compile_expr(&right);
                self.var_define(binding, None);
            },

            Expr(ref expr) => match expr.node {
                ExprNode::String(_) => {
                    self.compile_expr(expr);
                    self.emit(OpCode::Print)
                },
                _ => self.compile_expr(expr)
            },

            Print(ref expr) => {
                self.compile_expr(expr);
                self.emit(OpCode::Print)
            },

            Block(ref body) => for s in body.iter() {
                self.compile_statement(s);
            }

            Fn(ref binding, ref patterns) => {
                // Binding var
                for pattern in patterns {
                    let mut name = binding.name.clone();

                    // Self::mangle_name(&mut name, &pattern.0);

                    let mangled = Binding::from(name, &binding);
                    self.compile_pattern(&mangled, pattern.clone());
                }
            }

            _ => unimplemented!()
        }
    }

    fn compile_pattern(&mut self, binding: &Binding, pattern: (Vec<Expr>, Statement)) {
        // Function
        self.var_define(binding, None);

        let mut params = Vec::new();

        for expr in pattern.0.iter() {
            match expr.node {
                ExprNode::Var(ref binding) => {
                    params.push(binding);
                }
                _ => ()
            }
        }

        let arity = params.len() as u8;

        self.start_function(&binding.name, arity, 1);
        
        for p in params {
            self.state_mut().add_local(p.name(), 0);
            self.state_mut().resolve_local(p.name());
        }

        let body = if let StatementNode::Block(body) = pattern.1.node {
            body
        } else {
            vec!(pattern.1)
        };

        for (i, statement) in body.iter().enumerate() {
            if i == body.len() - 1 {
                if let StatementNode::Expr(ref expr) = statement.node {
                    self.emit_return(Some(expr.clone()))
                }
            }

            self.compile_statement(statement);
        }

        self.state_mut().end_scope();

        let upvalues = self.state_mut().upvalues.clone();

        let func = self.end_function();
        let handle = self.heap.insert(Obj::Func(func));

        let value = Value::object(handle);
        let idx = self.chunk_mut().add_constant(value);

        self.emit(OpCode::Closure);
        self.emit_byte(idx);

        for upvalue in upvalues {
            self.emit_byte(
                if upvalue.is_local {
                    1
                } else {
                    0
                }
            );

            self.emit_byte(upvalue.index)
        }

        // BRUH SET VAR
    }

    fn var_define(&mut self, var: &Binding, constant: Option<u8>) {
        // If there's depth, it's a local
        if let Some(depth) = var.depth {
            self.state_mut().add_local(var.name(), depth);
            self.state_mut().resolve_local(var.name());
        } else {
            self.emit(OpCode::DefineGlobal);

            let idx = constant.unwrap_or_else(|| {
                self.string_constant(var.name())
            });

            self.emit_byte(idx)
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        use self::ExprNode::*;

        match expr.node {
            Number(n) => {
                self.emit(OpCode::Immediate);

                let value = Value::float(n).to_raw();
                let chunk = self.chunk_mut();

                chunk.write_u64(value)
            }
            
            Var(ref n) => self.var_get(n),

            Binary(ref left, ref op, ref right) => {
                use self::Op::*;

                match op {
                    And => {
                        self.compile_expr(left);

                        let short_circuit_jmp = self.emit_jze();

                        self.emit(OpCode::Pop);
                        self.compile_expr(right);

                        self.patch_jmp(short_circuit_jmp);
                    },

                    Nor => {
                        self.compile_expr(left);
                        self.emit(OpCode::Not);

                        let short_circuit_jmp = self.emit_jze();

                        self.emit(OpCode::Pop);
                        self.compile_expr(right);
                        self.emit(OpCode::Not);

                        self.patch_jmp(short_circuit_jmp);
                    },

                    Or => {
                        self.compile_expr(left);

                        let else_jmp = self.emit_jze();
                        let end_jmp = self.emit_jmp();

                        self.patch_jmp(else_jmp);
                        self.emit(OpCode::Pop);

                        self.compile_expr(right);

                        self.patch_jmp(end_jmp)
                    },

                    _ => {    
                        self.compile_expr(left);
                        self.compile_expr(right);

                        match op {
                            Add => self.emit(OpCode::Add),
                            Sub => self.emit(OpCode::Sub),
                            Mod => self.emit(OpCode::Mod),
                            Mul => self.emit(OpCode::Mul),
                            Div => self.emit(OpCode::Div),
                            Pow => self.emit(OpCode::Pow),

                            Eq  => self.emit(OpCode::Eq),
                            Lt  => self.emit(OpCode::Lt),
                            Gt  => self.emit(OpCode::Gt),

                            NEq => {
                                self.emit(OpCode::Eq);
                                self.emit(OpCode::Not)
                            }

                            GEq => {
                                self.emit(OpCode::Lt);
                                self.emit(OpCode::Not)
                            }

                            LEq => {
                                self.emit(OpCode::Gt);
                                self.emit(OpCode::Not)
                            }

                            _ => ()
                        }
                    }
                }
            }

            Not(ref expr) => {
                self.compile_expr(expr);
                self.emit(OpCode::Not)
            }

            Neg(ref expr) => {
                self.compile_expr(expr);
                self.emit(OpCode::Neg)
            }

            String(ref s) => {
                let idx = {
                    let chunk = self.states.last_mut().unwrap().func.chunk_mut();
                    chunk.string_constant(self.heap, s)
                };

                self.emit(OpCode::Const(idx))
            }

            Call(ref call, ref args) => {
                let mut arity = args.len() as u8;

                self.compile_expr(call);

                // Mangle that name.
                if let Var(ref binding) = call.node {
                    arity = str::parse(&binding.name.split("__").last().unwrap()).unwrap();

                    let mask = &binding.name.split("$").collect::<Vec<&str>>()[1..];

                    for (arg, mask) in args.iter().zip(mask) {
                        if &mask[..1] == "v" {
                            self.compile_expr(arg)
                        }
                    }
                } else {
                    for arg in args.iter() {
                        self.compile_expr(arg)
                    }
                }

                self.emit(OpCode::Call(arity))
            }

            _ => unimplemented!()
        }
    }

    fn var_get(&mut self, var: &Binding) {
        if var.is_upvalue() {
            let idx = self.resolve_upvalue(var.name());

            self.emit(OpCode::GetUpValue);
            self.emit_byte(idx);
        } else {
            // local time B)
            if var.depth.is_none() {
                self.emit(OpCode::GetGlobal);
                let idx = self.string_constant(var.name());
                self.emit_byte(idx)
            } else {
                let idx = self.state_mut().resolve_local(var.name());

                self.emit(OpCode::GetLocal);
                self.emit_byte(idx)
            }
        }
    }

    fn start_function(&mut self, name: &str, arity: u8, scope: usize) {
        let next_function = Func::new(name.to_string(), arity);
        let state = State::new( name.to_string(), next_function, scope);

        self.states.push(state)
    }

    fn end_function(&mut self) -> Func {
        let mut state: State = self.states.pop().expect("states can't be empty");

        self.locals_cache.extend(state.locals.clone());

        state.func.upvalue_count = state.upvalues.len();
        state.func
    }

    fn string_constant(&mut self, s: &str) -> u8 {
        let chunk = self.states.last_mut().unwrap().func.chunk_mut();

        chunk.string_constant(self.heap, s)
    }

    fn emit_return(&mut self, ret: Option<Expr>) {
        let state = self.state_mut();
        let initializer = state.func.name == "init";

        if initializer {
            self.emit(OpCode::GetLocal);
            self.emit_byte(0)
        } else if let Some(ref expr) = ret {
            self.compile_expr(expr)
        } else {
            self.emit(OpCode::Nil)
        }

        self.emit(OpCode::Ret)
    }

    fn emit(&mut self, op: OpCode) {
        let line = self.line();
        self.chunk_mut().write(op, line);
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk_mut().write_byte(byte);
    }

    fn state_mut(&mut self) -> &mut State {
        self.states.last_mut().expect("states can't be empty")
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        self.states.last_mut()
            .expect("states to be non-empty")
            .func
            .chunk_mut()
    }

    fn line(&mut self) -> usize {
        self.states.last_mut()
            .expect("states to be non-empty")
            .line
    }

    fn set_global(&mut self, name: &str) {
        self.emit(OpCode::SetGlobal);

        let idx = {
            let chunk = self.states.last_mut()
                .unwrap()
                .func
                .chunk_mut();

            chunk.string_constant(self.heap, name)
        };

        self.emit_byte(idx)
    }

    fn resolve_upvalue(&mut self, name: &str) -> u8 {
        let end = self.states.len() - 1;

        let (scope, mut index) =
            self.states[..end].iter_mut()
                .enumerate()
                .rev()
                .filter_map(|(i, enclosing)| {
                    enclosing.capture_local(name).map(|local| (i, local))
                })
                .next()
                .expect(&format!("[bruh] upvalue marked during resolution, but wasn't found: {}", name));


        index = self.states[scope + 1].add_upvalue(index, true);

        if scope >= self.states.len() - 2 {
            // if we're one scope from current function
            index
        } else {
            for enclosing in &mut self.states[scope + 2..] {
                index = enclosing.add_upvalue(index, false)
            }

            index
        }
    }

    fn emit_jze(&mut self) -> usize {
        let line = self.line();
        let chunk = self.chunk_mut();

        chunk.write(OpCode::JumpF, line);
        chunk.write_byte(0xff);
        chunk.write_byte(0xff);

        chunk.code.len() - 2
    }

    fn emit_jmp(&mut self) -> usize {
        let line = self.line();
        let chunk = self.chunk_mut();

        chunk.write(OpCode::Jump, line);
        chunk.write_byte(0xff);
        chunk.write_byte(0xff);
        chunk.code.len() - 2
    }

    fn emit_loop(&mut self, ip: usize) {
        let line = self.line();
        let chunk = self.chunk_mut();
        let sub = chunk.code.len() - ip + 3;

        let lo = (sub & 0xff) as u8;
        let hi = ((sub >> 8) & 0xff) as u8;

        chunk.write(OpCode::Loop, line);
        chunk.write_byte(lo);
        chunk.write_byte(hi);
    }

    fn chunk(&self) -> &Chunk {
        &self.states.last()
            .expect("states to be non-empty")
            .func
            .chunk
    }

    fn ip(&self) -> usize {
        self.chunk().code.len()
    }

    fn patch_jmp(&mut self, idx: usize) {
        let jmp = self.ip();
        let lo = (jmp & 0xff) as u8;
        let hi = ((jmp >> 8) & 0xff) as u8;

        self.chunk_mut().write_byte_at(idx, lo);
        self.chunk_mut().write_byte_at(idx + 1, hi);
    }
}