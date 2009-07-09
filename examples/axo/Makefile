
AUXCMOS=
#../../../ocsigen.dev/xmlp4/newocaml/simplexmlparser.cmo

SOURCES= AXOJs.ml AXOLang.ml AXOTree.ml AXOString.ml AXOCom.ml AXOHtml.ml AXOWidgets.ml AXOEvents.ml AXOToolkit.ml main.ml
CMOS=$(patsubst %.ml, %.cmo, $(SOURCES))

.PHONY: clean all depend

all: main.exe.uue vm.js

main.exe: $(CMOS) ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc $(AUXCMOS) $(CMOS) -o $@

%.cmi: %.mli ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -c $<

%.cmo: %.ml ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -c $<

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

#include .depend

depend:
	@CAMLLIB=../../rt/caml ocamldep $(SOURCES)> .depend


