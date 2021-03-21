# Language

It's just a language.

## Syntax-teaser

Coming to a repo and seeing nothing but a todo-list is kinda sad. Here is some syntax I just used for testing.

Executes flawlessly in 0.00s.

```
func fib -> {
    | 0 => 0
    | 1 => 1
    | n => fib(n - 1) + fib(n - 2)
}

print fib(12)
```

## Todo

- [x] Pretty fast single-pass parser/compiler.
- [x] Virtual machine and compact bytecode format.
- [x] Pattern-matching functions with name-mangling.
- [x] Rust FFI.
- [x] IMPORTANT: Fallback-function to get hard-to-predict values.
- [x] Goto and flow-control.
- [x] Nice errors.
- [x] Lists.
- [x] Dictionaries.
- [x] Loops.
- [x] Ecnomics.
    - [x] Random distributions.
    - [x] Sum and vector operations.
    - [x] List-related functions.
    - [x] Map function over list.
- [x] A whole simulation of simple transactions.
