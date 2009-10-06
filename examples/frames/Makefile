SOURCES=boot.ml events.ml style.ml geometry.ml main.ml
CMOS=$(patsubst %.ml, %.cmo, $(SOURCES))

.PHONY: clean all depend

all: main.exe.uue vm.js

main.exe: $(CMOS) ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -I ../../axo AXO.cma $(CMOS) -o $@

%.cmo: %.ml ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -I ../../axo/ -c $<

%.uue: %
	@echo "[UUENCODE] $@"
	@uuencode $^ stdout > $@

vm.js: ../../vm.js
	@echo "[CP] $@"
	@cp $< $@

../../vm.js: ../../rt/js/*.js
	@cd ../.. && $(MAKE) vm.js

 ../../rt/caml/stdlib.cma:  ../../rt/caml/*.ml*
	@cd ../.. && $(MAKE) rt/caml/stdlib.cma

clean:
	@echo "[CLEAN]"
	@rm -f *.cm* *~ *.exe *.uue vm.js

include .depend

depend:
	@CAMLLIB=../../rt/caml ocamldep $(SOURCES)> .depend