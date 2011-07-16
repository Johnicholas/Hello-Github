miniprolog: miniprolog.ml \
	message.cmi message.cmo \
	syntax.cmi syntax.cmo \
	solve.cmi solve.cmo \
	unify.cmo \
	parser.cmi parser.cmo \
	lexer.cmo
	ocamlc syntax.cmo message.cmo lexer.cmo parser.cmo unify.cmo solve.cmo miniprolog.ml -o miniprolog
TO_CLEAN += miniprolog miniprolog.cmi

message.cmi message.cmo: message.ml
	ocamlc -c message.ml
TO_CLEAN += message.cmi message.cmo

syntax.cmi syntax.cmo: syntax.ml
	ocamlc -c syntax.ml
TO_CLEAN += syntax.cmi syntax.cmo

solve.cmi solve.cmo: solve.ml unify.cmi syntax.cmi
	ocamlc -c solve.ml
TO_CLEAN += solve.cmi solve.cmo

unify.cmi unify.cmo: unify.ml syntax.cmi
	ocamlc -c unify.ml
TO_CLEAN += unify.cmi unify.cmo

parser.cmo: parser.ml
	ocamlc -c parser.ml
TO_CLEAN += parser.cmo

parser.cmi: parser.mli lexer.mli
	ocamlc -c parser.mli
TO_CLEAN += parser.cmi parser.cmo

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly
TO_CLEAN += parser.ml parser.mli

lexer.cmi lexer.cmo: lexer.ml
	ocamlc -c lexer.ml
TO_CLEAN += lexer.cmi lexer.cmo

lexer.ml lexer.mli: lexer.mll
	ocamllex lexer.mll
TO_CLEAN += lexer.ml lexer.mli

clean:
	rm -f $(TO_CLEAN)
.PHONY: clean


