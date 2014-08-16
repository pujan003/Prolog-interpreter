ocamlc type.ml
ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c tokenparse.ml
ocamlc -c -w -A unifier.ml
ocamlc -c -w -A evaluate.ml
ocamlc -c -w -A main.ml
ocamlc -o main type.cmo lexer.cmo parser.cmo tokenparse.cmo unifier.cmo evaluate.cmo main.cmo
./main $1
