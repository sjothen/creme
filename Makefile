OCAMLBLDFLAGS=nums.cma
OCAMLNLDFLAGS=nums.cmxa
SOURCES=src/creme.ml src/eval.ml src/lexer.mll src/reader.ml src/main.ml
RESULT=creme

all: native-code

-include OCamlMakefile
