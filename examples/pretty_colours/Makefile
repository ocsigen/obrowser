DIALECTS=js ocaml xml html
COPIES=syntax_common.ml  $(patsubst %,syntax_%.ml, $(DIALECTS))
SOURCES=$(COPIES) main.ml
CMOS=$(patsubst %.ml, %.cmo, $(SOURCES))

.PHONY: clean depend

all: main.exe.uue vm.js

main.exe: $(CMOS) ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc $(CMOS) -o $@

%.cmo: %.ml
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -c $<

%.uue: %
	@echo "[UUENCODE] $@"
	@uuencode $^ stdout > $@

vm.js: ../../vm.js
	@echo "[CP] $@"
	@cp $< $@

../../vm.js: ../../rt/js/*.js
	@cd ../.. && make vm.js

 ../../rt/caml/stdlib.cma:  ../../rt/caml/*.ml*
	@cd ../.. && make rt/caml/stdlib.cma

syntax_%.ml: ../../tutorial/syntax_%.ml
	@echo "[LINK] $@"
	@ln -s $< $@

depend: $(SOURCES)  ../../rt/caml/stdlib.cma
	CAMLLIB=../../rt/caml ocamldep $(SOURCES) >.depend

include .depend

clean:
	@echo "[CLEAN]"
	@rm -f *.cm* *~ *.exe *.uue $(COPIES) vm.js
