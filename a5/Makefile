UNITS=ast_factory eval main parse checker promise
MLS_WITHOUT_MLIS=ast types
MLS=$(UNITS:=.ml) $(MLS_WITHOUT_MLIS:=.ml)
OBJECTS=$(UNITS:=.cmo) $(MLS_WITHOUT_MLIS:=.cmo) parser.cmo
MLIS=$(UNITS:=.mli)
TEST=test.byte
RUN=run.byte
REPL=repl.byte
OCAMLBUILD=ocamlbuild -r -use-ocamlfind -menhir "menhir --table --explain"
PKGS=oUnit,str,camomile,lwt

build:
	$(OCAMLBUILD) $(OBJECTS)

default: all

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

run:
	$(OCAMLBUILD) $(RUN) && ./$(RUN)

repl:
	$(OCAMLBUILD) $(REPL) && ./$(REPL)

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip -r a5src.zip *.ml* rml _tags Makefile

docs: docs-public docs-private

docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private a5src.zip report
