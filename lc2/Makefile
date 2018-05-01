OCB=ocamlbuild
OCB_FLAGS=-use-ocamlfind -cflag "-annot"
MENHIROPT=-yaccflag --explain

all:
	$(OCB) $(OCB_FLAGS) tests.native

clean:
	$(OCB) -clean
