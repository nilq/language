use super::value::{*, object::Obj};
use super::heap::Heap;

#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Ret,
    Const(u8),
    Nil,
    False,
    True,
    Pop,
    GetLocal,
    SetLocal,
    CloseUpValue,
    SetUpValue,
    GetUpValue,
    SetGlobal,
    GetGlobal,
    Immediate,
    DefineGlobal,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Print,
    Closure,
    Call(u8),
    Jump,
    JumpF,
    Loop,
    Eq,
    Neg,
    Gt,
    Lt,
    Not,
    BAnd, // Boolean and. Not jumping.
}

impl OpCode {
    pub fn write(&self, buffer: &mut Vec<u8>) {
        use self::OpCode::*;

        let op = match *self {
            Ret => 0x00,
            Const(i) => {
                buffer.push(0x01);
                buffer.push(i);

                return
            },
            Nil          => 0x02,
            False        => 0x03,
            True         => 0x04,
            Pop          => 0x05,
            GetLocal     => 0x06,
            SetLocal     => 0x07,
            CloseUpValue => 0x08,
            SetUpValue   => 0x09,
            GetUpValue   => 0x0a,
            SetGlobal    => 0x0b,
            GetGlobal    => 0x0c,
            Immediate    => 0x0d,
            DefineGlobal => 0x0f,
            Add          => 0x10,
            Sub          => 0x11,
            Mul          => 0x12,
            Div          => 0x13,
            Mod          => 0x14,
            Pow          => 0x15,
            Print        => 0x16,
            Call(a)   => 0x17 + a,
            Closure      => 0x20,
            Jump         => 0x21,
            JumpF        => 0x22,
            Loop         => 0x23,

            Eq           => 0x24,
            Neg          => 0x25,
            Gt           => 0x26,
            Lt           => 0x27,
            Not          => 0x28,

            BAnd         => 0x29,
        };

        buffer.push(op)
    } 
}

#[macro_export]
macro_rules! decode_op {
    ($op:expr, $this:ident) => {
        match $op {
            0x00 => $this.ret(),
            0x01 => { let idx = $this.read_byte(); $this.constant(idx); }
            0x02 => $this.imm_nil(),
            0x03 => $this.imm_false(),
            0x04 => $this.imm_true(),
            0x05 => { $this.pop(); },
            0x06 => $this.get_local(),
            0x07 => $this.get_global(),
            0x08 => $this.close_upvalue(),

            0x09 => $this.set_upvalue(),
            0x0a => $this.get_upvalue(),
            0x0b => $this.set_global(),
            0x0c => $this.get_global(),

            0x0d => $this.immediate(),

            0x0f => $this.define_global(),


            0x10 => $this.add(),
            0x11 => $this.sub(),
            0x12 => $this.mul(),
            0x13 => $this.div(),
            0x14 => $this.modulo(),
            0x15 => $this.pow(),

            0x16 => $this.print(),

            a @ 0x17..=0x1f => {
                $this.call(a - 0x17)
            },

            0x20 => $this.closure(),
            0x21 => $this.jmp(),
            0x22 => $this.jze(),
            0x23 => $this.loopy(),

            0x24 => $this.eq(),
            0x25 => $this.neg(),
            0x26 => $this.gt(),
            0x27 => $this.lt(),
            0x28 => $this.not(),

            0x29 => $this.band(),

            _ => unreachable!()
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct Line {
    pub start: usize,
    pub line: usize,
}

#[derive(Clone)]
pub struct Chunk {
    pub name: String,
    constants: Vec<Value>,
    pub code: Vec<u8>,
    lines: Vec<Line>,
}

impl Chunk {
    pub fn new(name: String) -> Self {
        Chunk {
            name,
            constants: Vec::new(),
            code: Vec::new(),
            lines: Vec::new()
        }
    }

    pub fn write(&mut self, op: OpCode, line: usize) {
        self.add_line(line);
        op.write(&mut self.code);
    }

    pub fn write_byte(&mut self, byte: u8) {
        self.code.push(byte);
    }

    pub fn write_byte_at(&mut self, idx: usize, byte: u8) {
        self.code[idx] = byte;
    }

    pub fn write_u64(&mut self, val: u64) {
        (0..8).for_each(|i| self.write_byte(((val >> i * 8) & 0xFF) as u8))
    }

    pub fn read_byte(&self, idx: usize) -> u8 {
        self.code[idx]
    }

    pub fn read_u16(&self, idx: usize) -> u16 {
        let mut t = 0u16;
        let size = ::std::mem::size_of::<u16>();
        
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self.code[idx],
                &mut t as *mut u16 as *mut u8,
                size);
        }

        t.to_le()
    }

    pub fn read_u64(&self, idx: usize) -> u64 {
        let mut t = 0u64;
        let size = ::std::mem::size_of::<u64>();
        
        unsafe {
            ::std::ptr::copy_nonoverlapping(
                &self.code[idx],
                &mut t as *mut u64 as *mut u8,
                size);
        }

        t.to_le()
    }

    fn add_line(&mut self, line: usize) {
        match self.lines.last().cloned() {
            Some(last) if last.line >= line => return,
            _ => (),
        }

        self.lines.push(Line {
            start: self.code.len(),
            line: line,
        });
    }

    pub fn add_constant(&mut self, constant: Value) -> u8 {
        for (i, c) in self.constants.iter().enumerate() {
            if *c == constant {
                return i as u8;
            }
        }

        if self.constants.len() == 1028 {
            panic!("A chunk cannot have more than 1028 constants");
        }

        self.constants.push(constant);
        self.constants.len() as u8 - 1
    }

    pub fn string_constant(&mut self, heap: &mut Heap<Obj>, string: &str) -> u8 {
        for (i, c) in self.constants().enumerate() {
            let obj = c
                .as_object()
                .and_then(|o| heap.get(o))
                .and_then(|o| o.as_string());

            if let Some(s) = obj {
                if s == string {
                    return i as u8
                }
            }
        }

        let handle = heap.insert(Obj::String(string.to_owned()));
        self.add_constant(Value::object(handle))
    }

    pub fn line(&self, offset: usize) -> usize {
        let idx =
            self.lines
                .binary_search_by_key(&offset, |line_info| line_info.start)
                .map_err(|idx| idx - 1)
                .unwrap_or_else(|idx| idx);
        self.lines[idx].line
    }

    pub fn get_constant(&self, idx: u8) -> Option<&Value> {
        self.constants.get(idx as usize)
    }

    pub fn constants(&self) -> Constants {
        Constants::new(self.constants.iter())
    }

    pub fn get(&self, ip: usize) -> u8 {
        self.code[ip]
    }
}

impl AsRef<[u8]> for Chunk {
    fn as_ref(&self) -> &[u8] {
        &self.code[..]
    }
}

pub struct Constants<'c> {
    iter: ::std::slice::Iter<'c, Value>
}

impl<'c> Constants<'c> {
    fn new(iter: ::std::slice::Iter<'c, Value>) -> Self {
        Constants { iter }
    }
}

impl<'c> Iterator for Constants<'c> {
    type Item = Value;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|v| *v)
    }
}