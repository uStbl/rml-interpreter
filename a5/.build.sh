#!/bin/sh

ocamlbuild -r -use-ocamlfind -menhir "menhir --table --explain" $@
