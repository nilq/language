use rand::distributions::{Distribution, Uniform};
use rand::prelude::*;

use crate::vm::{heap::Heap, value::{Value, object::{Obj, List}, Variant}};

pub fn assert(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if !args[1].truthy() {
        panic!("assertion failed.")
    }

    Value::truelit()
}

pub fn gc(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    heap.clean();
    Value::nil()
}

pub fn len(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(ref mut list) = list.as_list() {            
            return Value::number(list.content.len() as f64)
        } else {
            panic!("can't get length of non-list")
        }
    } else {
        panic!("can't get length of non-list")
    }
}

pub fn sum(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(list) = list.as_list() {
            let mut sum = 0f64;

            for item in list.content.iter() {
                if let Variant::Number(n) = item.decode() {
                    sum += n
                } else {
                    panic!("can't sum non-float")
                }
            }

            Value::number(sum)
        } else {
            panic!("can't sum non-list")
        }
    } else {
        let mut sum = 0f64;
        for arg in args {
            if let Variant::Number(n) = arg.decode() {
                sum += n
            } else {
                panic!("can't sum non-float")
            }
        }

        Value::number(sum)
    }
}

pub fn shuffle(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if let Variant::Obj(handle) = args[1].decode() {
        let list = unsafe { heap.get_unchecked(handle) };

        if let Some(ref mut list) = list.as_list() {
            let mut content = list.content.clone();
            let mut rng = rand::thread_rng();

            content.shuffle(&mut rng);

            let list = Obj::List(List::new(content));
            let handle = heap.insert(list).into_handle();

            return Value::object(handle)
        } else {
            panic!("can't shuffle non-list")
        }
    } else {
        panic!("can't shuffle non-list")
    }
}

pub fn random(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    let mut rng = rand::thread_rng();
    let y: f64 = rng.gen();

    Value::number(y)
}