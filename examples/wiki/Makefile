.PHONY=all clean

all: vm.js
	@ocamlbuild -ocamlc 'OCAMLLIB=$(PWD)/../../rt/caml ocamlc' main.byte
	@uuencode main.byte stdout > main.uue

vm.js: ../../vm.js
	@echo "[CP] $@"
	@cp $< $@

../../vm.js: ../../rt/js/*.js
	@cd ../.. && $(MAKE) vm.js

clean:
	@ocamlbuild -clean
	@-/bin/rm -f *~
