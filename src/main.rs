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
func fib -> {
    | 0 => 0
    | 1 => 1
    | n => {
        fib(n - 1) + fib(n - 2)
    }
}

print "HOLY SHIT fucking damnnnnn: " + fib(12)
    "#.to_string();

    let lex = Token::lexer(&fib);
    let parser = Parser::new(lex.spanned().collect::<Vec<(Token, Span)>>(), &fib);

    if let Ok(ast) = parser.parse(&["assert", "assert$v__1"]) {
        // println!("{:#?}", ast);
        // println!("{}", func_test);

        let mut vm = Vm::new();
        vm.add_native("assert$v__1", prelude::assert, 1);

        vm.execute(ast, false);

        // println!("{:#?}", vm.globals);
        // println!("{:#?}", vm.stack);
    }
}
