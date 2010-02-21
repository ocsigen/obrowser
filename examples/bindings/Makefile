.PHONY: clean

all: main.exe.uue vm.js

main.exe: corner_click.cma main.ml ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc corner_click.cma main.ml -o $@

corner_click.cma: corner_click_stubs.o corner_click.cmo ../../rt/caml/stdlib.cma
	@echo "[OCAMLMKLIB] $@"
	@CAMLLIB=../../rt/caml ocamlmklib corner_click_stubs.o corner_click.cmo -o corner_click

%.o: %.c
	@echo "[CC] $@"
	@CAMLLIB=../../rt/caml ocamlc -c $<

%.cmo: %.ml ../../rt/caml/stdlib.cma
	@echo "[CC] $@"
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
	@rm -f *.cm* *~ *.cma *.o *.exe *.uue vm.js *.so *.dylib *.dll *.a
