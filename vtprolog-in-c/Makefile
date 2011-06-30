
test: test_vtprolog
	./test_vtprolog

test_vtprolog: test_vtprolog.c vtprolog_as_library.o
	gcc test_vtprolog.c vtprolog_as_library.o -o test_vtprolog

vtprolog_as_library.o: vtprolog.c
	gcc -c vtprolog.c -o vtprolog_as_library.o -DBUILD_AS_LIBRARY

clean:
	rm -rf *~ *.o test_vtprolog
