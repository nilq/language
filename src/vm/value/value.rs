use std::fmt::{Display, Debug};

use crate::vm::{
    value::object::Obj,
    heap::{Heap, Handle, TaggedHandle, Tag},
    trace::{Trace, Tracer}
};

use std::mem;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum HashVariant {
    Bool(bool),
    Int(i64),
    Str(String),
    Nil,
}

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub struct HashValue {
    pub variant: HashVariant
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Variant {
    Obj(Handle<Obj>),
    Number(f64),
    True,
    False,
    Nil,
}

impl Variant {
    pub fn to_hash(&self, heap: &Heap<Obj>) -> HashVariant {
        use self::Variant::*;

        match *self {
            Number(ref f) => {
                unsafe {
                    HashVariant::Int(
                        mem::transmute::<f64, i64>(*f)
                    )
                }
            },

            True  => HashVariant::Bool(true),
            False => HashVariant::Bool(false),

            Obj(ref n) => unsafe {
                HashVariant::Str(heap.get_unchecked(n).as_string().unwrap().clone().to_string())
            },

            Nil => HashVariant::Nil,
        }
    }
}

const TAG_TRUE:  u8 = 0x01;
const TAG_FALSE: u8 = 0x02;
const TAG_NIL:   u8 = 0x03;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Value {
    handle: TaggedHandle<Obj>,
}

impl Value {
    pub fn decode(&self) -> Variant {
        use self::Tag::*;

        match self.handle.clone().decode() {
            Number(n) => Variant::Number(n),
            Handle(n) => Variant::Obj(n),
            Tag(t) if t == TAG_TRUE  => Variant::True,
            Tag(t) if t == TAG_FALSE => Variant::False,
            Tag(t) if t == TAG_NIL   => Variant::Nil,
            Tag(t) => panic!("Unknown tag: {}", t)
        }
    }

    pub fn with_heap<'h>(&self, heap: &'h Heap<Obj>) -> WithHeap<'h, Self> {
        WithHeap::new(heap, *self)
    }

    pub unsafe fn from_raw(raw: u64) -> Self {
        Value {
            handle: TaggedHandle::from_raw(raw),
        }
    }

    pub fn to_raw(self) -> u64 {
        self.handle.to_raw()
    }

    pub fn number(number: f64) -> Self {
        Value {
            handle: TaggedHandle::from_float(number),
        }
    }

    pub fn truelit() -> Self {
        Value {
            handle: TaggedHandle::from_tag(TAG_TRUE),
        }
    }

    pub fn falselit() -> Self {
        Value {
            handle: TaggedHandle::from_tag(TAG_FALSE),
        }
    }

    pub fn truthy(&self) -> bool {
        match self.decode() {
            Variant::False | Variant::Nil => false,
            _ => true,
        }
    }

    pub fn nil() -> Self {
        Value {
            handle: TaggedHandle::from_tag(TAG_NIL),
        }
    }

    pub fn object(handle: Handle<Obj>) -> Self {
        Value {
            handle: TaggedHandle::from_handle(handle)
        }
    }

    pub fn as_object<'a>(&self) -> Option<Handle<Obj>> {
        match self.decode() {
            Variant::Obj(o) => Some(o),
            _ => None,
        }
    }
}

impl Trace<Obj> for Value {
    fn trace(&self, tracer: &mut Tracer<Obj>) {
        if let Variant::Obj(obj) = self.decode() {
            obj.trace(tracer);
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self.decode() {
            Variant::Nil => write!(f, "nil"),
            Variant::False => write!(f, "false"),
            Variant::True => write!(f, "true"),
            Variant::Number(n) => write!(f, "{:?}", n),
            Variant::Obj(o) => write!(f, "{:?}", o),
        }
    }
}

impl From<Handle<Obj>> for Value {
    fn from(handle: Handle<Obj>) -> Self {
        Value::object(handle)
    }
}

impl Into<Value> for f64 {
    fn into(self) -> Value {
        Value::number(self)
    }
}

impl Into<Value> for bool {
    fn into(self) -> Value {
        if self {
            Value::truelit()
        } else {
            Value::falselit()
        }
    }
}

pub struct WithHeap<'h, T> {
    pub heap: &'h Heap<Obj>,
    pub item: T,
}

impl<'h, T> WithHeap<'h, T> {
    pub fn new(heap: &'h Heap<Obj>, item: T) -> WithHeap<'h, T> {
        WithHeap { heap, item }
    }

    pub fn with<U>(&self, item: U) -> WithHeap<U> {
        WithHeap { heap: self.heap, item }
    }
}

impl<'h> Display for WithHeap<'h, Value> {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self.item.decode() {
            Variant::Nil => write!(f, "nil"),
            Variant::False => write!(f, "false"),
            Variant::True => write!(f, "true"),
            Variant::Number(n) => write!(f, "{}", n),
            Variant::Obj(o) => {
                let o = self.heap.get(o).ok_or(::std::fmt::Error)?;
                write!(f, "{}", self.with(o))
            },
        }
    }
}
