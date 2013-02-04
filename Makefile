OCAMLBLDFLAGS=nums.cma
OCAMLNLDFLAGS=nums.cmxa
SOURCES=src/creme.ml src/eval.ml src/parser.mly src/lexer.mll src/main.ml
RESULT=creme

all: native-code

-include OCamlMakefile
