ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c tokenize.ml
ocamlc -o tokenize parser.cmo scanner.cmo tokenize.cmo