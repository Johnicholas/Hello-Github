SRCS = Integer.C BF.C main.C Lexer.C Parser.C BFOptimize.C BFCompile.C BFInterpreter.C BFMakeInsns.C 
INCLS = GenericError.h Integer.h BF.h Lexer.h Parser.h version.h BFImpl.h NumericLimits.h
CPLUSPLUS = g++
CC = gcc
LIBS = -lgmp
FLAGS = -Wall -O3
RM = rm
TAR = tar
GREP = grep
SED = sed
CMP = cmp

all: bfdb bflen bfref

bfdb: $(SRCS) $(INCLS)
	$(CPLUSPLUS) $(FLAGS) -o bfdb $(SRCS) $(LIBS)

bfref: bfref.c
	$(CC) $(FLAGS) -o bfref bfref.c

bflen: bflen.c
	$(CC) $(FLAGS) -o bflen bflen.c

clean:
	$(RM) -f bflen bfdb bfref

# This target must be built with -s to avoid printing junk at the start of the
# tar file.
tar:
	$(TAR) cvf - test.b test.input *.1 *.c *.C *.h Makefile README COPYING

test:	bfdb
	$(RM) -f __test.output __test.actual-output __test-output.c __test-output-[2345678].c
	$(GREP) '^%-' test.input | $(SED) 's/^%-//' > __test.output
	./bfdb -n <test.input >__test.actual-output
	if ( $(CMP) -s __test.output __test.actual-output ) then echo 'Test 1 successful.'; else echo 'Test 1 failed.'; fi
	if ( $(CMP) -s __test-output.c test-output.c ) then echo 'Test 2 successful.'; else echo 'Test 2 failed.'; fi
	if ( $(CMP) -s __test-output-2.c test-output-2.c ) then echo 'Test 3 successful.'; else echo 'Test 3 failed.'; fi
	if ( $(CMP) -s __test-output-3.c test-output-3.c ) then echo 'Test 4 successful.'; else echo 'Test 4 failed.'; fi
	if ( $(CMP) -s __test-output-4.c test-output-4.c ) then echo 'Test 5 successful.'; else echo 'Test 5 failed.'; fi
	if ( $(CMP) -s __test-output-5.c test-output-5.c ) then echo 'Test 6 successful.'; else echo 'Test 6 failed.'; fi
	if ( $(CMP) -s __test-output-6.c test-output-6.c ) then echo 'Test 7 successful.'; else echo 'Test 7 failed.'; fi
	if ( $(CMP) -s __test-output-7.c test-output-7.c ) then echo 'Test 8 successful.'; else echo 'Test 8 failed.'; fi
	if ( $(CMP) -s __test-output-8.c test-output-8.c ) then echo 'Test 9 successful.'; else echo 'Test 9 failed.'; fi
	$(RM) -f __test.output __test.actual-output __test-output.c __test-output-[2345678].c
