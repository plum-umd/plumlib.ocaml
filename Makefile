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

test-package: clean reinstall
	cd sample && \
    ocamlbuild -use-ocamlfind use.native || \
	  ./use.native

test: tests.native
	./tests.native

install: default
	opam pin add -y plumlib .

uninstall:
	opam pin remove -y plumlib

reinstall: uninstall install

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
