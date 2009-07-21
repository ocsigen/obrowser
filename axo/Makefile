

SOURCES= AXOJs.ml AXOLang.ml AXOCom.ml AXOHtml.ml AXOStyle.ml AXOEvents.ml AXOWidgets.ml AXOToolkit.ml
CMOS=$(patsubst %.ml, %.cmo, $(SOURCES))
TEST= main.cmo

.PHONY: clean all depend

all: main.exe.uue vm.js AXO.cma

main.exe: ../rt/caml/stdlib.cma $(CMOS) $(TEST)
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc $(CMOS) $(TEST) -o $@

AXO.cma: ../rt/caml/stdlib.cma $(CMOS)
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc -a -linkall -o $@ $(CMOS)

%.cmi: %.mli ../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc -c $<

%.cmo: %.ml ../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc -c $<

$(TEST): $(CMOS)
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc -c main.ml

%.uue: %
	@echo "[UUENCODE] $@"
	@uuencode $^ stdout > $@

vm.js: ../vm.js
	@echo "[CP] $@"
	@cp $< $@

../vm.js: ../rt/js/*.js
	@cd .. && $(MAKE) vm.js

../rt/caml/stdlib.cma:  ../rt/caml/*.ml*
	@cd .. && $(MAKE) rt/caml/stdlib.cma

clean:
	@echo "[CLEAN]"
	@rm -f *.cm* *~ *.exe *.uue vm.js

depend:
	@CAMLLIB=../rt/caml ocamldep $(SOURCES)> .depend


