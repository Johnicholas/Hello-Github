CXXFLAGS += -Wall
CXXFLAGS += -Werror
CXXFLAGS += -ansi
CXXFLAGS += -pedantic
LDFLAGS=-L/usr/local/lib
LDLIBS=-ltolua++ -llua -llualib -ldl -lm

COMPILE=g++ $(CXXFLAGS) $(LDFLAGS)

default: test

and.o: and.h tree.h
OBJECTS += and.o

or.o: or.h tree.h
OBJECTS += or.o

isset.o: isset.h tree.h context.h
OBJECTS += isset.o

equals.o: equals.h tree.h context.h
OBJECTS += equals.o

action.o: action.h tree.h context.h
OBJECTS += action.o

question_context.o: question_context.h context.h
OBJECTS += question_context.o

TO_CLEAN += $(OBJECTS)

short_circuit_expressions.c: short_circuit_expressions.pkg
	tolua++ short_circuit_expressions.pkg >short_circuit_expressions.c
TO_CLEAN += short_circuit_expressions.c

artificial_example_test: $(OBJECTS)
TO_BUILD += artificial_example_test

dependency_injection: dependency_injection.cpp \
	$(OBJECTS) \
	short_circuit_expressions.c
	$(COMPILE) dependency_injection.cpp $(OBJECTS) $(LDLIBS) -o $@
TO_BUILD += dependency_injection

all: $(TO_BUILD)
.PHONY: all

test: dependency_injection artificial_example.lua
	./dependency_injection artificial_example.lua

clean:
	rm -rf $(TO_BUILD) $(TO_CLEAN)
.PHONY: clean
