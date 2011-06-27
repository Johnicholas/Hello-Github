test_constraints: test_constraints.c libconstraints.a constraints.h
	gcc test_constraints.c -o test_constraints -lconstraints -L.
EXECUTABLES += test_constraints

libconstraints.a: constraints.o
	ar cr libconstraints.a constraints.o
LIBRARIES += libconstraints.a

constraints.o: constraints.c constraints.h
	gcc -c constraints.c -o constraints.o
TEMPORARY_FILES += constraints.o

.PHONY: all clean

all: $(EXECUTABLES) $(LIBRARIES)

clean:
	rm -rf $(EXECUTABLES) $(LIBRARIES) $(TEMPORARY_FILES)

