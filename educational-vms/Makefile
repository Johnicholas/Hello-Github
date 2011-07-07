EXECUTABLES= august buzzard.2 sbi short

CFLAGS=-ansi

all: $(EXECUTABLES)
.PHONY: all

clean:
	rm -f $(EXECUTABLES)

august: august.c
	${CC} ${CFLAGS} -DZ=60000 -D'T=m[s]' -D'P=m[s++]' \
	    -D'L=m[p++]' -D'g=getchar()' -DE=else -DW=while -D'B=m[p++]' \
	    -DI=if -DR='s=s+l/2;T=r;I(l%2)s++' -D'X=m[s-' \
	    -D'D=Q(13,-)Q(14,*)Q(15,/)Q(16,%)Q(6,==)Q(7,!=)Q(8,<)C(1,r=P;m[T]=r;T=r)C(9,r=P;m[T]=r;s++)' \
	    august.c -o august

