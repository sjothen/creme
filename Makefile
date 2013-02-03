OCAMLBLDFLAGS=nums.cma
OCAMLNLDFLAGS=nums.cmxa
SOURCES=creme.ml eval.ml parser.mly lexer.mll main.ml
RESULT=creme

all: native-code

-include OCamlMakefile
