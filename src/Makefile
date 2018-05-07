OCB=ocamlbuild
OCB_FLAGS=-use-ocamlfind -cflag "-annot" -tag 'debug'

all:
	$(OCB) $(OCB_FLAGS) tests.native
	$(OCB) $(OCB_FLAGS) tests.byte

clean:
	$(OCB) -clean
