use crate::vm::{heap::Heap, value::{Value, object::Obj}};

pub fn print(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    println!("{}", args[1].with_heap(heap));
    Value::nil()
}

pub fn assert(heap: &mut Heap<Obj>, args: &[Value]) -> Value {
    if !args[0].truthy() {
        panic!("assertion failed.")
    }

    Value::truelit()
}