
#SIMPLEXMLPARSER=simplexmlparser.cmo

SOURCES= AXOJs.ml AXOLang.ml AXOCom.ml AXOHtml.ml AXOStyle.ml AXOEvents.ml AXOWidgets.ml AXOToolkit.ml main.ml
CMOS=$(patsubst %.ml, %.cmo, $(SOURCES))

.PHONY: clean all depend

all: main.exe.uue vm.js

main.exe: $(SIMPLEXMLPARSER) $(CMOS) ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -verbose $(SIMPLEXMLPARSER) $(CMOS) -o $@

$(SIMPLEXMLPARSER):
	cp ~/src/ocsigen.dev/xmlp4/newocaml/simplexmlparser.cm{o,i} .

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
	@rm -f *.cm* *~ *.exe *.uue vm.js simplexmlparser.cm{o,i}

#include .depend

depend:
	@CAMLLIB=../../rt/caml ocamldep $(SOURCES)> .depend


