CFLAGS=-ansi

default: test
.PHONY: default

# objects

OBJS += table.o

TO_CLEAN += $(OBJS)

# executables

table_driver: $(OBJS)
TO_CLEAN += table_driver

# convenience targets

test: table_driver table_driver_test_script.txt table_driver_gold_output.txt
	./table_driver <table_driver_test_script.txt | diff -u - table_driver_gold_output.txt
.PHONY: test

clean:
	rm -rf $(TO_CLEAN)


