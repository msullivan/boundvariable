OCAMLC=ocamlopt

all: um um-fast

um: um.ml
	$(OCAMLC) -o um um.ml

um-fast: um-fast.ml
	$(OCAMLC) -o um-fast um-fast.ml

um-faster: lib/enum.ml lib/dynArray.ml um-faster.ml
	$(OCAMLC) -I lib -o um-faster lib/enum.mli lib/enum.ml lib/dynArray.mli lib/dynArray.ml um-faster.ml

clean:
	-rm um um-fast um-faster *.cmo *.cmi *.cmx *.o
	-rm lib/*.cmo lib/*.cmi lib/*.cmx lib/*.o
