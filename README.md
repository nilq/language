# a language yet to be named

## Syntax-teaser

Coming to a repo and seeing nothing but a todo-list is kinda sad. Here is some syntax I just used for testing.

Executes flawlessly in 0.00s.

```
func boo -> {
    | 0 => "bruh what " + "working"
    | a b => a + b
    | 0 b => "hurra lol: " + b
    | a b c => "nice: " +  a * b * c
}

print boo(0)
print boo(0 "ah")
print boo(10 10)
print boo(3 3 3)
```

## Todo

- [x] Pretty fast single-pass parser/compiler.
- [x] Virtual machine and compact bytecode format.
- [x] Pattern-matching functions with name-mangling.
- [ ] Rust FFI.
- [ ] Goto and flow-control.
- [ ] Lazy evaluation/compilation.
- [ ] Nice errors.
- [ ] More compact bytecode-output.
- [ ] `polar`-datatypes.