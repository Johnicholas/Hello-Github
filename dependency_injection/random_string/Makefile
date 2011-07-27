CXXFLAGS+=-Wall
CXXFLAGS+=-Werror
CXXFLAGS+=-ansi
CXXFLAGS+=-pedantic
LDFLAGS=-L/usr/local/lib
LDLIBS=-ltolua++ -llua -llualib -ldl -lm

COMPILE=g++ $(CXXFLAGS) $(LDFLAGS)

default: all

and_language.o: \
	and_language.h \
	language.h \
	language_implementation.h \
	language_interface.h \
	num.h \
	and_language.cpp
	$(COMPILE) -c and_language.cpp -o and_language.o
OBJECTS += and_language.o

or_language.o: \
	or_language.h \
	language.h \
	language_implementation.h \
	language_interface.h \
	num.h \
	or_language.cpp
	$(COMPILE) -c  or_language.cpp -o or_language.o
OBJECTS += or_language.o

singleton_language.o: \
	singleton_language.h \
	language.h \
	language_implementation.h \
	language_interface.h \
	num.h \
	singleton_language.cpp
	$(COMPILE) -c  singleton_language.cpp -o singleton_language.o
OBJECTS += singleton_language.o

bignum.o: \
	xp.h \
	num.h \
	bignum.cpp
	$(COMPILE) -c  bignum.cpp -o bignum.o
OBJECTS += bignum.o

language.o: \
	language_interface.h \
	num.h \
	language.h \
	singleton_language.h \
	or_language.h \
	and_language.h \
	language.cpp
	$(COMPILE) -c  language.cpp -o language.o
OBJECTS += language.o

xp.o: \
	xp.h \
	xp.cpp
	$(COMPILE) -c  xp.cpp -o xp.o
OBJECTS += xp.o

random_string: driver.cpp $(OBJECTS)
	$(COMPILE) driver.cpp $(OBJECTS) -o random_string
TO_BUILD += random_string

random_string.c: random_string.pkg
	tolua++ random_string.pkg >random_string.c
TO_CLEAN += random_string.c

lua_random_string: lua_driver.cpp $(OBJECTS) random_string.c
	$(COMPILE) lua_driver.cpp $(OBJECTS) $(LDLIBS) -o $@
TO_BUILD += lua_random_string

all: $(TO_BUILD)
.PHONY: all

clean:
	rm -f ./{,doc,testsuite{,/testdata}}/*{~,.o} $(TO_CLEAN)
.PHONY: clean
