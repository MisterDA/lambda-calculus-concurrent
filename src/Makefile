OCB=ocamlbuild
OCB_FLAGS=-use-ocamlfind -cflag "-annot" -tag 'debug' -use-menhir

all:
	$(OCB) $(OCB_FLAGS) run.native
        # $(OCB) $(OCB_FLAGS) tests.byte

clean:
	$(OCB) -clean
