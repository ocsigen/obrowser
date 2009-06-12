OBROWSERDIR := $(shell ocamlfind query obrowser)

all:
	OCAMLLIB=$(OBROWSERDIR) ocamlbuild main.byte
	uuencode main.byte stdout > main.uue
