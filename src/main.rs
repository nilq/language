#![feature(allocator_api)]

extern crate colored;

use logos::{Logos, Span};

mod prelude;

mod lexer;
use lexer::token::Token;

mod parser;
use parser::ast::*;
use parser::parser::*;

mod vm;
use vm::{
    chunk::*,
    vm::Vm
};

mod compiler;
use compiler::Compiler;

mod error;

fn main() {
    let test = r#"
global a = .0000001b

let b = 1.0

"nu er vi så bare her så bare :-))"

global c = 1 + 2

let d = 1.
global e = 01 + d * 2b

global lol = "hey tisser"

{
    print lol
}
"#;

    let func_test = r#"
func boo -> {
    | 0 => "bruh what " + "working"
    | a b => a + b
    | 0 b => "hurra lol: " + b
    | a b c => "nice: " + a * b * c
}

assert(not boo(0)  != "bruh what working")
assert(boo(0 "ah") == "hurra lol: ah")
assert(boo(10 10)  == 20)
assert(boo(3 3 3)  == "nice: " + 3^3)
"#;

    let fib = r#"
global func fib -> {
    | 0 => 1
    | 1 => 1
    | n => fib(n - 1) + fib(n - 2)
}

func fibs_brother -> {
    | n => if n < 2 {
               return 1
           } else {
               return fibs_brother(n - 1) + fibs_brother(n - 2)
           }
}

assert(fibs_brother(5) == fib(5))
    "#.to_string();

    let jump_map = r#"
print shuffle([1, 2, 3, 4])

global i      = 0
global values = [0, 0, 0, 0, 0]

repeat 5 {
    values[i] = random()
    print values[i]
    i = i + 1
}
    "#.to_string();

    let lex = Token::lexer(&jump_map);
    let parser = Parser::new(lex.spanned().collect::<Vec<(Token, Span)>>(), &jump_map);

    if let Ok(ast) = parser.parse(&[
        "assert", "assert$v__1",
        "sum", "sum$v__1",
        "len", "len$v__1",
        "gc",
        "random",
        "shuffle", "shuffle$v__1",
    ]) {
        println!("{:#?}", ast);
        // println!("{}", func_test);

        let mut vm = Vm::new();

        vm.add_native("assert$v__1", prelude::assert, 1);
        vm.add_native("sum$v__1", prelude::sum, 1);
        vm.add_native("len$v__1", prelude::len, 1);
        vm.add_native("gc", prelude::gc, 0);
        vm.add_native("shuffle$v__1", prelude::shuffle, 1);
        vm.add_native("random", prelude::random, 0);

        vm.execute(ast, true);

        // println!("{:#?}", vm.globals);
        // println!("{:#?}", vm.stack);
    }
}
