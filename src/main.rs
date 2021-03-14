extern crate colored;

use logos::{Logos, Span};

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
    | 0 => "bruh what " + "working tis"
    | a b => a + b
    | 0 b => "hurra lol: " + b
    | a b c => "nice: " +  a * b * c
}

print boo(0)
print boo(0 "ah")
print boo(10 10)
print boo(3 3 3)
"#;

    let lex = Token::lexer(func_test);
    let parser = Parser::new(lex.spanned().collect::<Vec<(Token, Span)>>());

    let ast = parser.parse().unwrap();
    // println!("{:#?}", ast);
    // println!("{}", func_test);

    let mut vm = Vm::new();
    vm.execute(ast, false);

    // println!("{:#?}", vm.globals);
    // println!("{:#?}", vm.stack);
}
