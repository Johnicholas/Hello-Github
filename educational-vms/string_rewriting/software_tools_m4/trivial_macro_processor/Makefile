# configuration

CFLAGS=-ansi

default: test
.PHONY: default

# objects

OBJS += table.o
OBJS += io.o

TO_CLEAN += $(OBJS)

# executables

table_driver: $(OBJS)
TO_CLEAN += table_driver

io_driver: $(OBJS)
TO_CLEAN += io_driver

trivial: $(OBJS)
TO_CLEAN += trivial

# convenience targets

test_table_driver: table_driver table_driver_test_script.txt table_driver_gold_output.txt
	./table_driver <table_driver_test_script.txt | diff -u - table_driver_gold_output.txt
.PHONY: test_table_driver

test_io_driver: io_driver io_driver_test_script.txt
	./io_driver <io_driver_test_script.txt
.PHONY: test_io_driver

test_trivial: trivial trivial_test_script.txt trivial_gold_output.txt
	./trivial <trivial_test_script.txt | diff -u - trivial_gold_output.txt
.PHONY: test_trivial

test: test_table_driver test_io_driver test_trivial
.PHONY: test

clean:
	rm -rf $(TO_CLEAN)


