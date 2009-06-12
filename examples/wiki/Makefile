.PHONY=all clean

all: vm.js
	@OCAMLLIB=`pwd`/../../rt/caml ocamlbuild main.byte
	@uuencode main.byte stdout > main.uue

vm.js: ../../vm.js
	@echo "[CP] $@"
	@cp $< $@

../../vm.js: ../../rt/js/*.js
	@cd ../.. && make vm.js

clean:
	@ocamlbuild -clean
	@-/bin/rm *~
