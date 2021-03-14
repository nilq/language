use super::value::{*, object::{Obj, Closure, UpValue}};
use super::heap::{Heap, Handle};
use super::chunk::{Chunk, OpCode};
use super::disassembler::Disassembler;

use fnv::FnvBuildHasher;

#[macro_use]
use crate::decode_op;
use crate::{compiler::{Compiler}, parser::ast::Statement};

use std::mem;
use std::collections::HashMap;

const STACK_SIZE: usize = 6969;
const HEAP_GROWTH: usize = 2;

macro_rules! binary_op {
    ($self:ident, $op:tt) => {
        let b = $self.pop();
        let a = $self.pop();

        if let (Variant::Number(a), Variant::Number(b)) = (a.decode(), b.decode()) {
            let c = a $op b;
            $self.push(c.into());

            return
        }
    }
}

pub struct Frame {
    closure: Handle<Obj>,
    ip: usize,
    stack_start: usize,
}

impl Frame {
    pub fn new(closure: Handle<Obj>, stack_start: usize) -> Self {
        Frame {
            closure,
            ip: 0,
            stack_start,
        }
    }

    pub fn read_byte(&mut self) -> u8 {
        let ip = self.ip;
        self.ip += 1;
        self.with_chunk(|c| c.read_byte(ip))
    }

    pub fn read_u16(&mut self) -> u16 {
        let ip = self.ip;
        self.ip += 2;
        self.with_chunk(|c| c.read_u16(ip))
    }

    pub fn read_u64(&mut self) -> u64 {
        let ip = self.ip;
        self.ip += 8;
        self.with_chunk(|c| c.read_u64(ip))
    }

    pub fn read_constant_at(&mut self, idx: u8) -> Value {
        self.with_chunk(|c| *c.get_constant(idx).expect("[bruh moment] Invalid constant index"))
    }

    pub fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        self.read_constant_at(idx)
    }

    pub fn with_chunk<F, T>(&self, fun: F) -> T
        where
            F: FnOnce(&Chunk) -> T
    {
        unsafe {
            let closure = self.closure
                .get_unchecked()
                .as_closure()
                .expect("[bruh moment] Closure check failed.");
            fun(&closure.func.chunk)
        }
    }
}

pub struct Vm {
    pub heap: Heap<Obj>,
    pub stack: Vec<Value>,
    pub frames: Vec<Frame>,
    pub open_upvalues: Vec<UpValue>,
    pub globals: HashMap<String, Value, FnvBuildHasher>,
}

impl Vm {
    pub fn new() -> Self {
        Vm {
            stack: Vec::with_capacity(STACK_SIZE),
            heap: Heap::new(),
            frames: Vec::with_capacity(256),
            open_upvalues: Vec::with_capacity(16),
            globals: HashMap::with_hasher(FnvBuildHasher::default()),
        }
    }

    pub fn execute(&mut self, ast: Vec<Statement>, debug: bool) {
        let func = {
            let mut compiler = Compiler::new(&mut self.heap);
            compiler.compile(ast)
        };

        if debug {
            println!("\ncompiled: {:?}", func.chunk.code);
            let dis = Disassembler::new(&func.chunk, &self.heap);
            dis.disassemble();
        }

        let closure = Closure::new(func, Vec::new());
        let value = self.allocate(Obj::Closure(closure));

        self.push(Value::object(value));
        self.call(0);
        self.run();
    }

    fn run(&mut self)  {
        while !self.frames.is_empty() {
            let inst = self.read_byte();
            decode_op!(inst, self)
        }
    }

    fn define_global(&mut self) {
        let var = self.frame_mut().read_constant()
            .as_object()
            .map(|o| self.deref(o))
            .and_then(|o| o.as_string())
            .cloned()
            .expect("expected constant to be a string value");
        
        let lhs = self.stack.pop().unwrap();

        self.globals.insert(var, lhs);
    }

    fn get_global(&mut self) {
        let global = self.frame_mut()
            .read_constant()
            .as_object()
            .map(|o| self.deref(o))
            .and_then(|o| o.as_string())
            .expect("[bruh moment] `GetGlobal` requires a string identifier");
        
        if let Some(value) = self.globals.get(global).cloned() {
            println!("PUSHING {:#?} of {}", value, global);
            self.push(value)
        } else {
            self.runtime_error(&format!("[bruh moment] undefined global variable: `{}`", global.clone()))
        }
    }

    fn set_global(&mut self) {
        let handle = self.frame_mut().read_constant()
            .as_object()
            .filter(|&o| self.deref(o).as_string().is_some())
            .expect("[bruh moment] expected constant to be a string value");
    
        let var = unsafe {
            handle.get_mut_unchecked()
                .as_string()
                .unwrap()
        };

        let value = *self.stack.last().unwrap();

        if let Some(slot) = self.globals.get_mut(var) {
            *slot = value
        } else {
            self.globals.insert(var.clone(), value);
        }
    }

    fn current_closure(&mut self) -> &mut Closure {
        let handle = self.frame_mut().closure;
        self.deref_mut(handle)
            .as_closure_mut()
            .expect("[epic bruh moment] invalid closure")
    }

    fn set_upvalue(&mut self) {
        let value = self.peek();
        let idx = self.frame_mut().read_byte();
        let closure = self.current_closure();
        let res = closure.get(idx as usize).set(value);

        if let Err(i) = res {
            self.stack[i] = value
        }
    }

    fn get_upvalue(&mut self) {
        let idx = self.frame_mut().read_byte();
        let value = self.current_closure()
            .get(idx as usize)
            .get()
            .unwrap_or_else(|i| self.stack[i]);
        
        self.push(value)
    }

    fn get_local(&mut self) {
        let start = self.frame().stack_start;
        let idx = self.read_byte() as usize;
        let val = self.stack[start + idx];

        self.push(val)
    }

    fn set_local(&mut self) {
        let val = self.peek();
        let start = self.frame().stack_start;
        let idx = self.read_byte() as usize;

        self.stack[start + idx] = val
    }

    fn constant(&mut self, idx: u8) {
        let val = self.frame_mut().read_constant_at(idx);
        self.push(val)
    }

    fn call_closure(&mut self, handle: Handle<Obj>, arity: u8) {
        let closure = self.deref(handle)
            .as_closure()
            .expect("redundant cast to succeed");

        let last = self.stack.len();
        let frame_start = if last < arity as usize { 0 } else { last - (arity + 0) as usize };

        if closure.func.arity != arity {
            self.runtime_error(&format!("arity mismatch: {} != {} @ {}", closure.func.arity, arity, closure.func.name.clone()))
        }

        let frame = Frame::new(handle, frame_start);
        self.frames.push(frame);
    }

    fn call(&mut self, arity: u8) {
        let last = self.stack.len();

        let frame_start = if last < arity as usize { 0 } else { last - (arity + 1) as usize };

        let callee = self.stack.remove(frame_start).decode();

        if let Variant::Obj(handle) = callee {
            use self::Obj::*;

            match unsafe { self.heap.get_unchecked(handle) } {
                Closure(_) => {

                    self.call_closure(handle, arity)
                },

                _ => todo!()
            }
        } else {
            panic!("You are calling: {:?}", callee)
        }
    }

    fn runtime_error(&self, err: &str) {
        eprintln!("[error]: {}.", err);
        for frame in self.frames.iter().rev() {
            let ip = frame.ip;
            frame.with_chunk(|chunk| {
                let name = &chunk.name;
                let line = chunk.line(ip);
                eprintln!("         at [line {}] in {}", line, name);
            });
        }
        ::std::process::exit(1);
    }

    fn add(&mut self) {
        let b = self.pop();
        let a = self.pop();

        use self::Variant::*;

        match (a.decode(), b.decode()) {
            (Number(a), Number(b)) => return self.push((a + b).into()),
            (Obj(a), Obj(b)) => {
                let a = self.deref(a).as_string().unwrap();
                let b = self.deref(b).as_string().unwrap();

                let new = self.allocate(self::Obj::String(format!("{}{}", a, b)));

                return self.push(new.into())
            },
            (Obj(a), Number(b)) => {
                let a = self.deref(a).as_string().unwrap();

                let new = self.allocate(self::Obj::String(format!("{}{}", a, b)));

                return self.push(new.into())
            },
            (Number(a), Obj(b)) => {
                let b = self.deref(b).as_string().unwrap();

                let new = self.allocate(self::Obj::String(format!("{}{}", a, b)));

                return self.push(new.into())
                
            },
            _ => {}
        }
    }

    fn sub(&mut self) {
        binary_op!(self, -);
    }

    fn mul(&mut self) {
        binary_op!(self, *);
    }

    fn modulo(&mut self) {
        binary_op!(self, %);
    }

    fn div(&mut self) {
        binary_op!(self, /);
    }

    fn print(&mut self) {
        let value = self.pop();
        println!("{}", value.with_heap(&self.heap))
    }

    fn closure(&mut self) {
        let value = self.frame_mut().read_constant();
        let function = value.as_object()
            .map(|o| self.deref(o))
            .and_then(|o| o.as_func())
            .cloned()
            .expect("closure expected function argument");

        let mut upvalues = Vec::new();

        for _ in 0 .. function.upvalue_count {
            let is_local = self.read_byte() > 0;
            let idx = self.read_byte() as usize;
            let upvalue = if is_local {
                self.capture_upvalue(idx)
            } else {
                self.current_closure().get(idx)
            };

            upvalues.push(upvalue)
        }

        let closure = Closure::new(function, upvalues);
        let value = self.allocate(Obj::Closure(closure)).into();

        self.push(value)
    }

    fn pow(&mut self) {
        let b = self.pop();
        let a = self.pop();

        if let (Variant::Number(a), Variant::Number(b)) = (a.decode(), b.decode()) {
            let c = a.powf(b);

            self.push(c.into());
        }
    }

    fn push(&mut self, value: Value) {
        if self.stack.len() == STACK_SIZE {
            panic!("STACK OVERFLOW >:(");
        }

        self.stack.push(value);
    }

    fn capture_upvalue(&mut self, idx: usize) -> UpValue {
        let offset = self.frame().stack_start + idx;

        self.open_upvalues.iter().rev()
            .find(|&up| {
                up.as_local().map(|i| i == offset).unwrap_or(false)
            })
            .cloned()
            .unwrap_or_else(|| {
                let up = UpValue::new(offset);
                self.open_upvalues.push(up.clone());
                up
            })
    }

    fn frame(&self) -> &Frame {
        self.frames.last().expect("frames to be nonempty")
    }

    fn frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("frames to be nonempty")
    }

    fn read_byte(&mut self) -> u8 {
        self.frame_mut().read_byte()
    }

    fn read_u16(&mut self) -> u16 {
        self.frame_mut().read_u16()
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().expect("stack to be nonempty")
    }

    fn allocate(&mut self, object: Obj) -> Handle<Obj> {
        let handle = self.heap.insert(object);
        handle
    }

    fn deref(&self, o: Handle<Obj>) -> &Obj {
        unsafe { self.heap.get_unchecked(o) }
    }

    fn deref_mut(&mut self, o: Handle<Obj>) -> &mut Obj {
        self.heap.get_mut_unchecked(o)
    }

    fn immediate(&mut self) {
        let raw = self.frame_mut().read_u64();
        let val = unsafe { Value::from_raw(raw) };

        self.push(val)
    }

    fn imm_nil(&mut self) {
        self.push(Value::nil());
    }

    fn imm_true(&mut self) {
        self.push(Value::truelit());
    }

    fn imm_false(&mut self) {
        self.push(Value::falselit());
    }

    fn jmp(&mut self) {
        self.frame_mut().ip = self.read_u16() as usize
    }

    fn jze(&mut self) {
        let ip = self.read_u16();
        if !self.peek().truthy() {
            self.frame_mut().ip = ip as usize
        }
    }

    fn loopy(&mut self) {
        self.frame_mut().ip -= self.read_u16() as usize
    }

    fn close_upvalue(&mut self) {
        let end = self.stack.len() - 1;

        self.close_upvalues(end);
        self.pop();
    }

    fn close_upvalues(&mut self, stack_end: usize) {
        let mut open_upvalues = Vec::new();

        mem::swap(&mut self.open_upvalues, &mut open_upvalues);

        for mut up in open_upvalues {
            if up.get().map_err(|i| i >= stack_end).is_err() {
                up.close(|i| self.stack[i]);
                
                self.open_upvalues.push(up)
            }
        }
    }

    fn ret(&mut self) {
        if let Some(frame) = self.frames.pop() {
            let return_value = self.pop();

            if frame.stack_start < self.stack.len() {
                self.close_upvalues(frame.stack_start)
            }
            
            self.stack.truncate(frame.stack_start);
            self.push(return_value);
        } else {
            self.runtime_error("can't return from top-level");
        }
    }

    fn peek(&mut self) -> Value {
        *self.stack.last().expect("stack to be nonempty")
    }
}