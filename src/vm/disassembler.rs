#[macro_use]
use crate::decode_op;

use super::chunk::Chunk;
use super::heap::{Heap, Handle};
use super::value::{Value, Variant, object::Obj};

use colored::Colorize;

pub struct Disassembler<'c> {
    offset: usize,
    line: usize,
    chunk: &'c Chunk,
    heap: &'c Heap<Obj>,
}

impl<'c> Disassembler<'c> {
    pub fn new(chunk: &'c Chunk, heap: &'c Heap<Obj>) -> Self {
        Disassembler {
            offset: 0,
            line: 0,
            chunk,
            heap,
        }
    }

    pub fn disassemble(mut self) {
        let bytes = self.chunk.as_ref();

        println!();
        let name = format!("{}:", self.chunk.name);
        eprint!("{}", name.magenta());

        while self.offset < bytes.len() {
            self.disassemble_instruction();
        }

        println!();
    }

    fn disassemble_instruction(&mut self) {
        let line = self.chunk.line(self.offset);
        if self.line == line {
        } else {
            self.line = line;
        }
        let inst = self.read_byte();
        println!();
        let off = format!("{:04} | ", self.offset);

        eprint!("{}", off.red());
        decode_op!(inst, self);
    }

    fn constant(&mut self, idx: u8) {
        let val = self.chunk.get_constant(idx);
        eprint!("CONSTANT {} {:?}", idx, val);
    }

    fn ret(&self) { eprint!("ret"); }
    fn print(&self) { eprint!("print"); }
    fn add(&self) { eprint!("add"); }
    fn sub(&self) { eprint!("sub"); }
    fn mul(&self) { eprint!("mul"); }
    fn modulo(&self) { eprint!("mod"); }
    fn pow(&self) { eprint!("pow"); }
    fn div(&self) { eprint!("div"); }
    fn neg(&self) { eprint!("neg"); }
    fn not(&self) { eprint!("not"); }
    fn eq(&self) { eprint!("eq"); }
    fn band(&self) { eprint!("band"); }
    fn gt(&self) { eprint!("gt"); }
    fn lt(&self) { eprint!("lt"); }
    fn pop(&self) { eprint!("pop"); }

    fn list(&mut self) {
        eprint!("list");
        self.read_byte();
    }

    fn index(&mut self) {}

    fn dict(&mut self) {
        eprint!("dict");
        self.read_byte();
    }


    fn jmp(&mut self) {
        let offset = self.offset - 1;
        let ip = self.read_u16();
        eprint!("jump {} -> {}", offset, ip);
    }

    fn jze(&mut self) {
        let offset = self.offset - 1;
        let ip = self.read_u16();
        eprint!("jump_f {} -> {}", offset, ip);
    }

    fn loopy(&mut self) {
        let sub = self.read_u16() as usize;
        eprint!("loop {} -> {}", self.offset, self.offset - sub);
    }

    fn get_global(&mut self) {
        let val = self.read_constant();
        eprint!("get_global {}", val.with_heap(self.heap));
    }

    fn set_global(&mut self) {
        let val = self.read_constant();
        eprint!("set_global {}", val.with_heap(self.heap));
    }

    fn define_global(&mut self) {
        let val = self.read_constant();
        eprint!("def_global {}", val.with_heap(self.heap));
    }

    fn get_local(&mut self) {
        let val = self.read_byte();
        eprint!("get_local {}", val);
    }

    fn set_local(&mut self) {
        let val = self.read_byte();
        eprint!("set_local {}", val);
    }

    fn immediate(&mut self) {
        self.offset += 8;
        let b1 = self.chunk.get(self.offset - 8) as u64;
        let b2 = self.chunk.get(self.offset - 7) as u64;
        let b3 = self.chunk.get(self.offset - 6) as u64;
        let b4 = self.chunk.get(self.offset - 5) as u64;
        let b5 = self.chunk.get(self.offset - 4) as u64;
        let b6 = self.chunk.get(self.offset - 3) as u64;
        let b7 = self.chunk.get(self.offset - 2) as u64;
        let b8 = self.chunk.get(self.offset - 1) as u64;
        let raw = b1   +
            (b2 << 8)  +
            (b3 << 16) +
            (b4 << 24) +
            (b5 << 32) +
            (b6 << 40) +
            (b7 << 48) +
            (b8 << 56);
        let val = unsafe { Value::from_raw(raw) };
        eprint!("float {}", val.with_heap(self.heap));
    }

    fn imm_nil(&self) {
        eprint!("nil");
    }

    fn imm_true(&self) {
        eprint!("true");
    }

    fn imm_false(&self) {
        eprint!("false");
    }

    fn call(&self, arity: u8) {
        eprint!("call_{}", arity);
    }

    fn close_upvalue(&self) {
        eprint!("close_upvalue");
    }

    fn get_upvalue(&mut self) {
        let index = self.read_byte();
        eprint!("get_upvalue {}", index);
    }

    fn set_upvalue(&mut self) {
        let index = self.read_byte();
        eprint!("set_upvalue {}", index);
    }

    fn closure(&mut self) {
        let val = self.read_constant();
        let count = val
            .as_object()
            .and_then(|o| self.heap.get(o))
            .and_then(|o| o.as_func())
            .expect("[bruh] closure argument to be a function")
            .upvalue_count;

        println!("closure {} ", val.with_heap(self.heap));

        if let Variant::Obj(cl) = val.with_heap(self.heap).item.decode() {
            unsafe {
                let closure = cl.get_unchecked().as_func().unwrap();

                let dis = Disassembler::new(&closure.chunk, &self.heap);
                dis.disassemble()
            }
        }

        for _ in 0..count {
            let _is_local = self.read_byte() > 0;
            let _index = self.read_byte();
        }
    }

    fn read_byte(&mut self) -> u8 {
        self.offset += 1;
        self.chunk.as_ref()[self.offset - 1]
    }

    fn read_u16(&mut self) -> u16 {
        self.offset += 2;
        let lo = self.chunk.get(self.offset - 2) as u16;
        let hi = self.chunk.get(self.offset - 1) as u16;
        lo + (hi << 8)
    }

    fn read_constant(&mut self) -> Value {
        let idx = self.read_byte();
        *self.chunk.get_constant(idx).expect("invalid constant segment index")
    }
}
