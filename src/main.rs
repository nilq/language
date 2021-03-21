#![feature(allocator_api)]

extern crate colored;
extern crate textplots;

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


use std::path::Path;
use std::fs::File;
use std::io::prelude::*;

fn run(code: &String) {
    let lex = Token::lexer(&code);
    let parser = Parser::new(lex.spanned().collect::<Vec<(Token, Span)>>(), &code);

    if let Ok(ast) = parser.parse(&[
        "assert", "assert$v__1",
        "sum", "sum$v__1",
        "len", "len$v__1",
        "gc",
        "random",
        "shuffle", "shuffle$v__1",
        "plot", "plot$v$v$v__3",
        "zeros", "zeros$v__1",
        "range", "range$v__1",
        "sorted", "sorted$v__1",
        "choose", "choose$v__1",
        "uniform", "uniform$v$v__2",
        "gaussian", "gaussian$v$v__2",
        "append", "append$v$v__2",
        "sample", "sample$v$v__2",
        "histogram", "histogram$v$v$v$v__4",

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
        vm.add_native("plot$v$v$v__3", prelude::plot, 3);
        vm.add_native("zeros$v__1", prelude::zeros, 1);
        vm.add_native("range$v__1", prelude::range, 1);
        vm.add_native("sorted$v__1", prelude::sorted, 1);
        vm.add_native("choose$v__1", prelude::choose, 1);
        vm.add_native("append$v$v__2", prelude::append, 2);
        vm.add_native("uniform$v$v__2", prelude::uniform, 2);
        vm.add_native("gaussian$v$v__2", prelude::gaussian, 2);
        vm.add_native("histogram$v$v$v$v__4", prelude::histogram, 4);
        vm.add_native("sample$v$v__2", prelude::random_sample, 2);

        vm.execute(ast, false);

        // println!("{:#?}", vm.globals);
        // println!("{:#?}", vm.stack);
    }
}

fn run_file(path: &str) {
    let display = Path::new(path).display();

    let mut file = match File::open(&path) {
        Err(why) => panic!("failed to open {}: {}", display, why),
        Ok(file) => file,
    };

    let mut s = String::new();

    match file.read_to_string(&mut s) {
        Err(why) => panic!("failed to read {}: {}", display, why),
        Ok(_) => run(&s),
    }
}

fn test() {
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
func make_population -> {
    | N mu => {
        global population = zeros(N)
        global i = 0

        while i < N {
            population[i] = gaussian(mu mu/5) 
            i = i + 1
        }

        return population
    }
}

func gini -> {
    | a => {
        global a__ = sorted(a)
        global n = len(a)

        global number = 0
        global i = 0
        while i < n {
            number = number + 2 * (i + 1) * a__[i]

            i = i + 1
        }

        global denom = n * sum(a__)

        return (number / denom) - (n + 1) / n
    }
}

func transaction -> {
    | A B => {
        global pot   = A + B
        global share = uniform(0 pot)

        return [share, pot - share]
    }

    | A B tax => {
        global tax_n = tax * (A + B)
        global yield = transaction(A B)

        return [
            tax_n / 2 + yield[0],
            tax_n / 2 + yield[1]
        ]
    }
}

func winner_takes_all -> {
    | A B => choose([[A + B, 0], [0, A + B]])
}

func anyone -> {
    | N => sample(len(N) 2)
}

func simulate -> {
    | N T type => {
        global t = 0
        while t < T + 1 {
            global ab = anyone(N)

            global merchant_a = N[ab[0]]
            global merchant_b = N[ab[1]]

            global yield = false # Initial
            
            if type == "winner takes all" {
                yield = winner_takes_all(merchant_a merchant_b)
            }

            if type == "random" {
                yield = transaction(merchant_a merchant_b)
            }

            if type == "tax" {
                yield = transaction(merchant_a merchant_b 0.5)
            }


            N[ab[0]] = yield[0]
            N[ab[1]] = yield[1]

            t = t + 1
        }

        return N
    }
}

global X_range = range(400)

func status -> {
    | population name => {
        print name
        print "Gini-koefficient for ny population:"
        print gini(population)
        print ""

        histogram(
            name
            X_range
            population
            32
        )
    }
}

global SIM_STEPS = 50

func sim_random -> {
    | N mu => {
        global population     = make_population(N mu)
        global new_population = simulate(population SIM_STEPS "random")

        status(new_population "Tilfældige transaktioner")
    }
}

func sim_tax -> {
    | N mu rate => {
        global population     = make_population(N mu)
        global new_population = simulate(population SIM_STEPS "tax")

        status(new_population "Beskattede, tilfældige transaktioner")
    }
}

func sim_winner -> {
    | N mu => {
        global population     = make_population(N mu)
        global new_population = simulate(population SIM_STEPS "winner takes all")

        status(new_population "Winner takes all")

        return true
    }
}

sim_random(400 100)
print ""
print ""

sim_tax(400 100 0.4)
print ""
print ""

sim_winner(400 100)
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
        "plot", "plot$v$v$v__3",
        "zeros", "zeros$v__1",
        "range", "range$v__1",
        "sorted", "sorted$v__1",
        "choose", "choose$v__1",
        "uniform", "uniform$v$v__2",
        "gaussian", "gaussian$v$v__2",
        "append", "append$v$v__2",
        "sample", "sample$v$v__2",
        "histogram", "histogram$v$v$v$v__4",

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
        vm.add_native("plot$v$v$v__3", prelude::plot, 3);
        vm.add_native("zeros$v__1", prelude::zeros, 1);
        vm.add_native("range$v__1", prelude::range, 1);
        vm.add_native("sorted$v__1", prelude::sorted, 1);
        vm.add_native("choose$v__1", prelude::choose, 1);
        vm.add_native("append$v$v__2", prelude::append, 2);
        vm.add_native("uniform$v$v__2", prelude::uniform, 2);
        vm.add_native("gaussian$v$v__2", prelude::gaussian, 2);
        vm.add_native("histogram$v$v$v$v__4", prelude::histogram, 4);
        vm.add_native("sample$v$v__2", prelude::random_sample, 2);
        

        vm.execute(ast, true);

        // println!("{:#?}", vm.globals);
        // println!("{:#?}", vm.stack);
    }
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    if args[1] == "test" {
        return test()
    }

    for arg in args[1..].iter() {
        run_file(arg)
    }
}