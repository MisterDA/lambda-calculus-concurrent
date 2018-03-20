OCB=ocamlbuild
OCB_FLAGS=-use-ocamlfind -cflag "-annot"
MENHIROPT=-yaccflag --explain

all:
	$(OCB) $(OCB_FLAGS) interpreter.native

clean:
	$(OCB) -clean
