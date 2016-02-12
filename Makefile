.PHONY: clean default install test

OPTS = -use-ocamlfind
OPTOPTS = -ocamlopt 'ocamlopt -inline 20'

BASE = ocamlbuild $(OPTS)
BYTE = $(BASE)
NATIVE = $(BASE) $(OPTOPTS)
LIBS = a cma cmi cmo cmx cmxa

default: clean
	for ext in $(LIBS); do \
    $(BYTE) PLUM.$$ext || exit 1 ; \
  done

test: tests.native
	./tests.native
	cd sample && \
    ocamlbuild -use-ocamlfind use.native && \
	  ./use.native

install:
	opam pin add -y plumlib .

uninstall:
	opam pin remove -y plumlib

%.native : **/%.ml
	$(NATIVE) $@

%.byte : **/%.ml
	$(BYTE) $@

%.d.byte : **/%.ml
	$(BYTE) $@

%: %.native **/%.ml
	rm -f $@ $<
	ln -s _build/src/$< $@

clean:
	ocamlbuild -clean
	rm -f *.byte *.native *.out
	cd sample && ocamlbuild -clean
