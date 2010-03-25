.PHONY: clean

all: main.exe.uue obj_gen.exe.uue vm.js

main.exe: corner_click.cma main.ml ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc corner_click.cma main.ml -o $@

corner_click.cma: corner_click_stubs.o corner_click.cmo ../../rt/caml/stdlib.cma
	@echo "[OCAMLMKLIB] $@"
	@CAMLLIB=../../rt/caml ocamlmklib corner_click_stubs.o corner_click.cmo -o corner_click

obj_gen.exe: object_bindings_gen.ml ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc object_bindings_gen.ml -o $@

pa_object_bindings.cmo: pa_object_bindings.ml
	ocamlc -c -I +camlp4 -I +camlp4/Camlp4Printers -I +camlp4/Camlp4Parsers -pp camlp4of $<

object_bindings_gen.js: object_bindings_gen.ml
object_bindings_gen.ml: object_bindings.ml pa_object_bindings.cmo
	camlp4of pa_object_bindings.cmo -js object_bindings_gen.js -impl $< -o $@

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
	@rm -f *.cm* *~ *.cma *.o *.exe *.uue vm.js *.so *.dylib *.dll *.a object_bindings_gen.js object_bindings_gen.ml
