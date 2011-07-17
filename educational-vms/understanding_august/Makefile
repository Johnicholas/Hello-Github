EXECUTABLES= august

CFLAGS=-ansi

all: $(EXECUTABLES)
.PHONY: all

clean:
	rm -f $(EXECUTABLES)

test: august
	cat august.oo fac.oc | ./august | ./august

august: august.c
	cc ${CFLAGS} august.c -o august


