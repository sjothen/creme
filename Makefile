#all:
#	ocamlc -c creme.ml
#	ocamllex lexer.mll
#	ocamlyacc parser.mly
#	ocamlc -c parser.mli
#	ocamlc -c lexer.ml
#	ocamlc -c parser.ml
#	ocamlc -o main creme.cmo parser.cmo lexer.cmo

SOURCES=creme.ml parser.mly lexer.mll
RESULT=creme

-include OCamlMakefile
