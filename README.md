# Sémantique des Langages de Programmation

Antonin Décimo <antonin.decimo@gmail.com>

Made with OCaml. Requires
[ocamlbuild](https://github.com/ocaml/ocamlbuild) to compile.
A parser is provided, which uses [Menhir]() and [ocamllex]().

The syntax is similar to that of OCaml, but comparison operators have
been replaced by `==` and `!=` for equality and inequality. You may
also need more parentheses.

There are two schedulers available, the *round robin*, and the
*random*. Their codes are `RRI` and `RI`, respectively, and must be
provided to the interpreter.

``` shell
make              # build
./run.native <sched> <file>

# example
./run.native RRI myprog.lc
./run.native RI myprog.lc
```

The Abstract Syntax Tree is described in `ast.ml`. There are examples
in `tests.ml`, as well as utility functions in both `tests.ml` and
`lc.ml` modules. `lc.ml` contains the cps transformation function, and
the interpreter which is parameterized by a scheduler, implemented in
`sched.ml`.

```
.
├── ast.ml
├── lc.ml
├── Makefile
├── sched.ml
└── tests.ml
```
