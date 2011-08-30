# config

default: test
.PHONY: default

# code generation

josephus.c: keith_randall.py josephus_example.txt
	python keith_randall.py <josephus_example.txt >$@
	astyle $@
TO_CLEAN += josephus.c

simple_1.c: keith_randall.py simple_1_example.txt
	python keith_randall.py <simple_1_example.txt >$@
	astyle $@
TO_CLEAN += simple_1.c

simple_2.c: keith_randall.py simple_2_example.txt
	python keith_randall.py <simple_2_example.txt >$@
	astyle $@
TO_CLEAN += simple_2.c

minsky_register_machine.c minsky_register_machine.h: minsky_register_machine.lemon
	lemon minsky_register_machine.lemon
TO_CLEAN += minsky_register_machine.c minsky_register_machine.h

# executables

josephus: josephus.c
TO_CLEAN += josephus

simple_1: simple_1.c
TO_CLEAN += simple_1

simple_2: simple_2.c
TO_CLEAN += simple_2

mrm: mrm_driver.c minsky_register_machine.o
	gcc $^ -o $@
TO_CLEAN += mrm

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

test: $(TESTS)
.PHONY: test $(TESTS)

clean:
	rm -f $(TO_CLEAN)
.PHONY: clean
