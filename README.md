# Sémantique des Langages de Programmation

Antonin Décimo <antonin.decimo@gmail.com>

Made with OCaml. Requires
[ocamlbuild](https://github.com/ocaml/ocamlbuild) to compile.

``` shell
make              # build
./tests.native    # run
```

The Abstract Syntax Tree is described in `ast.ml`. There are examples
in `tests.ml`, as well as utility functions in both `tests.ml` and
`lc.ml` modules. `lc.ml` contains the cps transformation function, and
the interpreter which is parametrized by a scheduler, implemented in
`sched.ml`.

```
.
├── ast.ml
├── lc.ml
├── Makefile
├── sched.ml
└── tests.ml
```
