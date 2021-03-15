use std::rc::Rc;
use std::cell::RefCell;
use std::fmt::{Display, Debug};

use super::super::{chunk::Chunk, heap::Heap};
use super::value::{Value, WithHeap};

macro_rules! impl_as (
    ($name:ident, $typ:ident) => {
        pub fn $name(&self) -> Option<&$typ> {
            if let Obj::$typ(ref o) = *self {
                Some(o)
            } else {
                None
            }
        }
    }
);

pub enum Obj {
    String(String),
    Func(Func),
    Closure(Closure),
    NativeFunction(NativeFunction),
}

impl Obj {
    impl_as!(as_string, String);
    impl_as!(as_closure, Closure);
    impl_as!(as_func, Func);

    pub fn as_closure_mut(&mut self) -> Option<&mut Closure> {
        if let Obj::Closure(ref mut o) = *self {
            Some(o)
        } else {
            None
        }
    }

    pub fn native_fn(name: &str, arity: u8, function: fn(&mut Heap<Obj>, &[Value]) -> Value) -> Self {
        Obj::NativeFunction(
            NativeFunction {
                name: name.into(),
                arity,
                function,
            },
        )
    }
}


impl Debug for Obj {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::Obj::*;

        match self {
            String(ref s) => write!(f, "{:?}", s),
            Func(ref fun) => write!(f, "<fn {:?}>", fun.name),
            Closure(ref cl) => write!(f, "<closure {:?}>", cl.func.name),
            NativeFunction(ref na) => write!(f, "<ffi {:?}>", na.name),
        }
    }
}

impl<'h, 'a> Display for WithHeap<'h, &'a Obj> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::Obj::*;

        match self.item {
            String(ref s) => write!(f, "{}", s),
            Func(ref fun) => write!(f, "<fn {:?}>", fun.name),
            Closure(ref cl) => write!(f, "<closure {:?}>", cl.func.name),
            NativeFunction(ref na) => write!(f, "<ffi {:?}>", na.name),
        }
    }
}

pub struct FuncBuilder {
    pub name: String,
    pub chunk: Chunk,
    arity: u8,
    pub upvalue_count: usize,
}

impl FuncBuilder {
    pub fn new(name: String, arity: u8) -> Self {
        let chunk = Chunk::new(name.clone());
        FuncBuilder { name, arity, chunk, upvalue_count: 0 }
    }

    pub fn build(self) -> Func {
        Func::new(self)
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

#[derive(Clone)]
pub struct Func {
    pub name: String,
    pub chunk: Chunk,
    pub arity: u8,
    pub upvalue_count: usize,
}

impl Func {
    fn new(builder: FuncBuilder) -> Self {
        Func {
            name: builder.name,
            arity: builder.arity,
            chunk: builder.chunk,
            upvalue_count: builder.upvalue_count,
        }
    }

    pub fn raw(name: String, arity: u8) -> Self {
        Func {
            name: name.clone(),
            chunk: Chunk::new(name),
            arity,
            upvalue_count: 0,
        }
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
}

#[derive(Clone)]
pub struct UpValue {
    inner: Rc<RefCell<Result<Value, usize>>>,
}

impl UpValue {
    pub fn new(local: usize) -> Self {
        UpValue {
            inner: Rc::new(RefCell::new(Err(local))),
        }
    }

    pub fn close<F: FnOnce(usize) -> Value>(&mut self, f: F) {
        let mut inner = self.inner.borrow_mut();

        if let Err(e) = *inner {
            *inner = Ok(f(e))
        }
    }

    pub fn as_local(&self) -> Option<usize> {
        self.inner.borrow().err()
    }

    pub fn get(&self) -> Result<Value, usize> {
        self.inner.borrow().clone()
    }

    pub fn set(&mut self, value: Value) -> Result<(), usize> {
        let mut inner = self.inner.borrow_mut();
        (*inner)?;

        *inner = Ok(value);

        Ok(())
    }
}

#[derive(Clone)]
pub struct Closure {
    pub func: Func,
    pub upvalues: Vec<UpValue>,
}

impl Closure {
    pub fn new(func: Func, upvalues: Vec<UpValue>) -> Self {
        Closure {
            func,
            upvalues
        }
    }

    pub fn get(&self, i: usize) -> UpValue {
        self.upvalues[i].clone()
    }
}

#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub function: fn(&mut Heap<Obj>, &[Value]) -> Value,
}

