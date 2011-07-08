CFLAGS=-Wall -Werror -ansi -pedantic

test: test_vtprolog new_front_end example_input.pro
	./test_vtprolog
	./new_front_end <example_input.pro

vtprolog_lines.txt: vtprolog.grammar
	seq 1 1000 | random_string -g vtprolog.grammar | sed 's/P/./g' >$@

call_graph.pdf: call_graph.dot
	dot call_graph.dot -Tpdf >$@
TO_CLEAN += call_graph.pdf

new_front_end: new_vtprolog_lexer.o new_vtprolog_parser.o
	${CC} ${CFLAGS} $^ -o $@
TO_CLEAN += new_front_end

new_vtprolog_parser.o: new_vtprolog_parser.c new_vtprolog_parser.h
	${CC} ${CFLAGS} -c new_vtprolog_parser.c -o $@
TO_CLEAN += new_vtprolog_parser.o

new_vtprolog_parser.c new_vtprolog_parser.h: new_vtprolog_parser.lemon
	lemon $^
TO_CLEAN += new_vtprolog_parser.{c,h}

new_vtprolog_lexer.o: new_vtprolog_lexer.c new_vtprolog_parser.h
	${CC} ${CFLAGS} -c new_vtprolog_lexer.c -o $@
TO_CLEAN += new_vtprolog_lexer.o

new_vtprolog_lexer.c: new_vtprolog_lexer.re
	re2c -i new_vtprolog_lexer.re >$@
TO_CLEAN += new_vtprolog_lexer.c

test_vtprolog: test_vtprolog.c vtprolog_as_library.o
	${CC} ${CFLAGS} test_vtprolog.c vtprolog_as_library.o -o $@
TO_CLEAN += test_vtprolog

vtprolog_as_library.o: vtprolog.c
	${CC} ${CFLAGS} -c $^ -DBUILD_AS_LIBRARY -o $@
TO_CLEAN += vtprolog_as_library.o

clean:
	rm -rf *~ ${TO_CLEAN}
