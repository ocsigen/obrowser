

SOURCES= lwt_preemptive.ml lwt_obrowser.ml
CMOS=$(patsubst %.ml, %.cmo, $(SOURCES))

LWTDIR=`ocamlfind query lwt`
OPTIONS=-I $(LWTDIR) -I ../axo


.PHONY: clean all depend

all:depend lwt_preemptive.cmi lwt_obrowser.cma

lwt_obrowser.cma: ../rt/caml/stdlib.cma $(CMOS)
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc -a -o $@ $(CMOS)

%.cmi: %.mli ../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc $(OPTIONS) -c $<

%.cmo: %.ml ../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc $(OPTIONS) -c $<

../rt/caml/stdlib.cma:  ../rt/caml/*.ml*
	@cd .. && $(MAKE) rt/caml/stdlib.cma

clean:
	@echo "[CLEAN]"
	@rm -f *.cm* *~ *.exe *.uue vm.js .depend

.depend:
	@CAMLLIB=../rt/caml ocamldep $(SOURCES)> .depend


include .depend
