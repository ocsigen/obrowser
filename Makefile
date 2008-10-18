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
.PHONY: tuto dist plugin

all: rt/caml/stdlib.cma vm.js tuto $(EXAMPLES_TARGETS) examples.html

%.example: 
	@echo "[EXAMPLE] $*"
	@cd examples/$* && make --no-print-directory

tuto:
	@echo "[TUTORIAL]"
	@cd tutorial && make --no-print-directory

plugin:
	@echo "[MOZILLA PLUGIN]"
	@cd rt/plugin/npapi && make --no-print-directory

examples.html:
	@echo "[EXAMPLES INDEX]"
	@echo "<body><html><ul>" > $@ ; \
	 for i in $(EXAMPLES) ; do echo "<li><a href='examples/$$i/index.html'>$$i</a></li>" >> $@ ; done ; \
	echo "</ul></body></html>" >> $@

rt/caml/stdlib.cma: $(wildcard rt/caml/*.ml) $(wildcard rt/caml/*.mli)
	cd rt/caml && make

vm.js: 	$(wildcard rt/js/*.js)	
	@echo "[CPP] $@"
	@cpp -DDEBUG -P -Irt/js rt/js/main.js vm.js

clean:
	@echo "[CLEAN]"
	@$(RM) -f rt/js/*~
	@for ex in $(EXAMPLES) ; do cd examples/$$ex && make clean && cd ../.. ; done
	@cd rt/caml && make clean
	@rm -f examples.html
	@cd tutorial && make clean
	@$(RM) -f *.exe *.cm* *.so *.a *.dylib *.o *~ *.uue \
                    vm.js server
	@$(RM) -f examples/*/*.exe examples/*/*.cm* examples/*/*.exe.uue \

dist: clean
	@if ! [ -d dist ] ; then mkdir dist ; fi
	@B=$$(basename $$(pwd)) ; D=$$(date "+%y%m%d.%H%M") ; \
	 cd .. ; tar cjf $$B.$$D.tbz2 --exclude="$$B/dist*" $$B ; mv $$B.$$D.tbz2 $$B/dist ; \
	 echo "[DIST] dist/$$B.$$D.tbz2"