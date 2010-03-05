# Caml Virtual Machine in JavaScript       
# (C) 2007 Benjamin Canou (Benjamin.Canou@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
EXAMPLES = $(patsubst examples/%,%, $(wildcard examples/*))
EXAMPLES_TARGETS = $(patsubst examples/%,%.example, $(wildcard examples/*))
OCAMLFIND = ocamlfind
.PHONY: tuto dist plugin lwt

all: rt/caml/stdlib.cma vm.js tuto $(EXAMPLES_TARGETS) examples.html AXO lwt

%.example: 
	@echo "[EXAMPLE] $*"
	@cd examples/$* && $(MAKE) --no-print-directory

AXO:
	@echo "[AXO]"
	@cd axo/ && $(MAKE) --no-print-directory

lwt:
	@echo "[LWT]"
	@cd lwt/ && $(MAKE) --no-print-directory

tuto:
	@echo "[TUTORIAL]"
	@cd tutorial && $(MAKE) --no-print-directory

plugin:
	@echo "[MOZILLA PLUGIN]"
	@cd rt/plugin/npapi && $(MAKE) --no-print-directory

examples.html:
	@echo "[EXAMPLES INDEX]"
	@echo "<body><html><ul>" > $@ ; \
	 for i in $(EXAMPLES) ; do echo "<li><a href='examples/$$i/index.html'>$$i</a></li>" >> $@ ; done ; \
	echo "</ul></body></html>" >> $@

rt/caml/stdlib.cma: $(wildcard rt/caml/*.ml) $(wildcard rt/caml/*.mli)
	cd rt/caml && $(MAKE)

vm.js: 	$(wildcard rt/js/*.js)	
	@echo "[CPP] $@"
	@cpp -DDEBUG -P -Irt/js rt/js/main.js vm.js

clean:
	@echo "[CLEAN]"
	@$(RM) -f rt/js/*~
	@cd rt/caml && $(MAKE) clean
	@rm -f examples.html
	@cd tutorial && $(MAKE) clean
	@$(RM) -f *.exe *.cm* *.so *.a *.dylib *.o *~ *.uue \
                    vm.js server
	@$(RM) -f examples/*/*.exe examples/*/*.cm* examples/*/*.exe.uue
	@for ex in $(EXAMPLES) ; do cd examples/$$ex && $(MAKE) clean && cd ../.. ; done
	@cd axo && $(MAKE) clean

dist: clean
	@if ! [ -d dist ] ; then mkdir dist ; fi
	@B=$$(basename $$(pwd)) ; D=$$(date "+%y%m%d.%H%M") ; \
	 cd .. ; tar cjf $$B.$$D.tbz2 --exclude="*.svn*" --exclude="$$B/dist*" $$B ; mv $$B.$$D.tbz2 $$B/dist ; \
	 echo "[DIST] dist/$$B.$$D.tbz2"

install:
	$(OCAMLFIND) install obrowser META vm.js rt/js/ffi.js rt/caml/stdlib.cma rt/caml/*.cmi rt/caml/std_exit.cmo axo/AXO.cma axo/AXO*.cmi lwt/lwt_obrowser.cma lwt/lwt_*.cmi
	install -m 644 rt/caml/dllstdlib.so `$(OCAMLFIND) -query obrowser`

uninstall:
	$(OCAMLFIND) remove obrowser
