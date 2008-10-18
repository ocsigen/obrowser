ML=$(wildcard *.ml)
#CMO=$(patsubst %.ml,%.cmo, $(ML))
OTHERS=array.cmo list.cmo char.cmo string.cmo sys.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  sort.cmo obj.cmo \
  hashtbl.cmo \
  set.cmo map.cmo stack.cmo queue.cmo buffer.cmo \
  printf.cmo format.cmo scanf.cmo \
  printexc.cmo \
  digest.cmo random.cmo callback.cmo \
  camlinternalOO.cmo oo.cmo camlinternalMod.cmo \
  thread.cmo mutex.cmo complex.cmo lexing.cmo \
  js.cmo graphics.cmo
.PHONY=clean depend

all: stdlib.cma std_exit.cmo stubs.c

stdlib.cma: stubs.o pervasives.cmo $(OTHERS)
	@echo "[MKLIB] $@"
	@CAMLLIB=. ocamlmklib $^ -o $(basename $@)

%.cmo: %.ml pervasives.cmo
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc -nostdlib -c $< -o $@

%.cmi: %.mli pervasives.cmi
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc -nostdlib -c $< -o $@

pervasives.cmo: pervasives.ml
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc -nostdlib -nopervasives -c $< -o $@

pervasives.cmi: pervasives.mli
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc -nostdlib -nopervasives -c $< -o $@

%.o: %.c
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc -nostdlib -c $< -o $@

stubs.c: $(ML)
	@echo "[STUBS] $<"
	@echo "// stubs extracted from $<" > $@
	@cat $(ML) | perl -e '%s=();s/\s*external\s+([^(][^:]+|\([^)]+\)):\s*([^=]+)=[^"]*"([^%][^"]+).*/\/\/ Caml name: $$1\n\/\/ Type:   $$2\nvoid $$3 () {\n  return ;\n}/ && (exists $$s{$$3} || ($$s{$$3}=true && print)) while(<>)' >> $@

#STUBS=$(patsubst %.ml,%-stubs.c, $(ML))

#stubs.c: $(STUBS)
#	@echo "[STUBS] $@"
#	@cat $^ > $@

#%-stubs.c: %.ml build-stubs/build-stubs.cmo
#	@echo "[STUBS] $<"
#	@camlp4o -o $@.cmo build-stubs/build-stubs.cmo $<

#build-stubs/build-stubs.cmo: build-stubs/build-stubs.ml
#	@echo "[CAMLC] $@"
#	@cd build-stubs && ocamlc -pp camlp4orf -c -I +camlp4 build-stubs.ml && cd ..
depend:
	@echo "[CAMLDEP]"
	@CAMLLIB=. ocamldep *.ml > .depend

include .depend

clean:
	@echo "[CLEAN]"
	@rm -f *.a *.cm* *~ .d stubs.c *.o *.so *-stubs.c build-stubs/build-stubs.cmo
