
call_graph.pdf: call_graph.dot
	dot call_graph.dot -Tpdf >call_graph.pdf

test: test_vtprolog new_front_end example_input.pro
	./test_vtprolog
	./new_front_end <example_input.pro

new_front_end: new_vtprolog_lexer.o new_vtprolog_parser.o
	gcc new_vtprolog_lexer.o new_vtprolog_parser.o -o new_front_end

new_vtprolog_parser.o: new_vtprolog_parser.c new_vtprolog_parser.h
	gcc -c new_vtprolog_parser.c -o new_vtprolog_parser.o

new_vtprolog_parser.c new_vtprolog_parser.h: new_vtprolog_parser.lemon
	lemon new_vtprolog_parser.lemon

new_vtprolog_lexer.o: new_vtprolog_lexer.c new_vtprolog_parser.h
	gcc -c new_vtprolog_lexer.c -o new_vtprolog_lexer.o

new_vtprolog_lexer.c: new_vtprolog_lexer.re
	re2c -i new_vtprolog_lexer.re >new_vtprolog_lexer.c

test_vtprolog: test_vtprolog.c vtprolog_as_library.o
	gcc test_vtprolog.c vtprolog_as_library.o -o test_vtprolog

vtprolog_as_library.o: vtprolog.c
	gcc -c vtprolog.c -o vtprolog_as_library.o -DBUILD_AS_LIBRARY

clean:
	rm -rf *~ *.o test_vtprolog
