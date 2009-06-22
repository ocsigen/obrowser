all: vm.js \
     tutorial.exe.uue \
     hello.exe.uue \
     hello_event.exe.uue \
     hello_dom.exe.uue \
     num_sections.exe.uue \
     inline_img.exe.uue \
     clock.exe.uue \
     colorpicker.exe.uue \
     prod_cons.exe.uue

tutorial.exe:  syntax_common.cmo \
                   syntax_xml.cmo \
                   syntax_js.cmo \
                   syntax_html.cmo \
                   syntax_ocaml.cmo \
                   tutorial.cmo
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc $^ -o $@

%.exe: %.ml ../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc $< -o $@

%.cmo: %.ml ../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../rt/caml ocamlc -c $<

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
	@rm -f *.cm* *~ *.uue *.exe vm.js
