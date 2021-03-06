#!/bin/bash

cp ../../../src/scanner.mll ./scanner.mll
cp ../../../src/parser.mly ./parser.mly
cp ../../../src/ast.ml ./ast.ml

ocamllex scanner.mll
ocamlyacc parser.mly
ocamlc -c ast.ml
ocamlc -c parser.mli
ocamlc -c scanner.ml
ocamlc -c parser.ml
ocamlc -c ScannerTest.ml
ocamlc -o ScannerTest parser.cmo scanner.cmo ScannerTest.cmo
