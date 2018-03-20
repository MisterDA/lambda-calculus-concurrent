OCB=ocamlbuild
OCB_FLAGS_DEFAULT=-use-menhir -use-ocamlfind -cflag "-annot"
MENHIROPT=-yaccflag --explain

all:
	$(OCB) $(OCB_FLAGS) $(MENHIROPT) interpreter.native

clean:
	$(OCB) -clean
