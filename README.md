# Scheme interpreter written in OCaml.

Interpreter for a subset of R5RS scheme. Written purely to practice using OCaml. 

Lexer and parser are generated using ocamllex and menhir, uses jbuilder to build.

To build:
```bash
jbuilder build repl.exe
```

To run:
``` bash
# Run on a test scheme file
_build/default/repl.exe test.scm
# Run repl
_build/default/repl.exe 
```

Currently implemented:
* Ints (as OCaml ints so fixed precision)
* Bools
* If expressions
* Pairs 
* Variable definition and assignment
* Function definition and lambdas
* Quotation
* Tail recursion (for free, thanks OCaml!)

Still to do:
* Proper tests
* Derived syntax such as let / cond
* More data types - floats, chars, strings, bignums
* Macros
* Continuations

