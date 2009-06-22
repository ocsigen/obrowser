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

CMOS=lexer.cmo tokens.cmo parser.cmo calc.cmo main.cmo

.PHONY: clean

all: main.exe.uue vm.js

main.exe: $(CMOS) ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc $(CMOS) -o $@

%.cmo: %.ml ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
	@CAMLLIB=../../rt/caml ocamlc -c $<

%.cmi: %.mli ../../rt/caml/stdlib.cma
	@echo "[OCAMLC] $@"
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


	@echo "[AR] $@"
	@CAMLLIB=../obrowser/rt/caml ocamlc -a $^ -o $@

%.ml: %.mll
	@echo "[LEX] $@"
	@ocamllex $<

lexer.ml: tokens.cmi

parser.cmo: parser.cmi

parser.mli: parser.ml

parser.ml: parser.mly
	@echo "[MENHIR] $@"
	@CAMLLIB=../../rt/caml menhir --infer --base parser --external-tokens Tokens tokens.mly parser.mly

tokens.mli: tokens.ml

tokens.ml: tokens.mly
	@echo "[MENHIR] $@"
	@CAMLLIB=../../rt/caml menhir --infer --base tokens --only-tokens tokens.mly

clean:
	@echo "[CLEAN]"
	@rm -f *.cm* *~ *.exe *.uue vm.js parser.ml parser.mli tokens.ml tokens.mli
