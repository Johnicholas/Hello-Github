# config

default: test
.PHONY: default

# code generation

josephus.c: keith_randall.py josephus_example.txt
	python keith_randall.py <josephus_example.txt >$@
	astyle $@
TO_CLEAN += josephus.c josephus.c.orig

simple_1.c: keith_randall.py simple_1_example.txt
	python keith_randall.py <simple_1_example.txt >$@
	astyle $@
TO_CLEAN += simple_1.c simple_1.c.orig

simple_2.c: keith_randall.py simple_2_example.txt
	python keith_randall.py <simple_2_example.txt >$@
	astyle $@
TO_CLEAN += simple_2.c simple_2.c.orig

MRM=minsky_register_machine
$(MRM).c $(MRM).h: $(MRM).lemon
	lemon $(MRM).lemon
TO_CLEAN += $(MRM).c $(MRM).h $(MRM).out

mrm_simple_1.c: mrm
	./mrm >mrm_simple_1.c
	astyle $@
TO_CLEAN += mrm_simple_1.c mrm_simple_1.c.orig

# object files

$(MRM).o: $(MRM).c $(MRM).h
TO_CLEAN += $(MRM).o

# executables

josephus: josephus.c
TO_CLEAN += josephus

simple_1: simple_1.c
TO_CLEAN += simple_1

simple_2: simple_2.c
TO_CLEAN += simple_2

mrm: mrm_driver.cpp minsky_register_machine.o
	g++ $^ -o $@
TO_CLEAN += mrm

mrm_simple_1: mrm_simple_1.c
TO_CLEAN += mrm_simple_1

# convenience

test_simple_example_1: simple_1
	./simple_1 | diff -u - simple_1_gold_output.txt
TESTS += test_simple_example_1

test_simple_example_2: simple_2
	./simple_2 | diff -u - simple_2_gold_output.txt
TESTS += test_simple_example_2

test_josephus: josephus
	./josephus | diff -u - josephus_example_gold_output.txt
TESTS += test_josephus

test_mrm_driver: mrm_simple_1
	./mrm_simple_1 | diff -u - simple_1_gold_output.txt
TESTS += test_mrm_driver

test: $(TESTS)
.PHONY: test $(TESTS)

clean:
	rm -f $(TO_CLEAN)
.PHONY: clean
