ML=$(wildcard *.ml)
#CMO=$(patsubst %.ml,%.cmo, $(ML))
CMOS= \
  obj.cmo lazy.cmo \
  jSOO_basic_io.cmo pervasives.cmo sys.cmo \
  array.cmo list.cmo hashtbl.cmo \
  char.cmo string.cmo \
  stdLabels.cmo arrayLabels.cmo listLabels.cmo stringLabels.cmo \
  set.cmo map.cmo stack.cmo queue.cmo buffer.cmo sort.cmo \
  int32.cmo int64.cmo nativeint.cmo \
  printf.cmo format.cmo scanf.cmo \
  arg.cmo  printexc.cmo \
  digest.cmo random.cmo callback.cmo \
  sys.cmo thread.cmo marshal.cmo \
  camlinternalOO.cmo oo.cmo camlinternalMod.cmo \
  mutex.cmo complex.cmo parsing.cmo lexing.cmo stream.cmo genlex.cmo \
  js.cmo graphics.cmo jSOO.cmo

.PHONY=clean depend exec_compflags

all: exec_compflags pervasives.cmi stdlib.cma std_exit.cmo stubs.c

exec_compflags:
	@chmod +x Compflags

stdlib.cma: stubs.o $(CMOS)
	@echo "[MKLIB] $@"
	@CAMLLIB=. ocamlmklib $^ -o $(basename $@)

%.cmo: %.ml
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc `./Compflags $@` -nostdlib -I . -c $< -o $@

%.cmi: %.mli
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc `./Compflags $@` -nostdlib -I . -c $< -o $@

%.o: %.c
	@echo "[CAMLC] $@"
	@CAMLLIB=. ocamlc `./Compflags $@` -nostdlib -c $< -o $@

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
	@CAMLLIB=. ocamldep -I . *.ml > .depend

include .depend

clean:
	@echo "[CLEAN]"
	@rm -f *.a *.cm* *~ .d stubs.c *.o *.so *-stubs.c build-stubs/build-stubs.cmo
