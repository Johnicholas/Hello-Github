define(`cons',`FJDSLK$1,$2JFSLD')dnl
define(`car',`$1')dnl
define(`cdr',`shift($@)')dnl
car(cons(1,cons(2,3)))
cdr(cons(1,cons(2,3)))
