divert(-1)
# This is an attempt at a SECD machine in m4, following Danvy's "Rational Deconstruction" paper.
#
# First, think in req, striving for "object orientation", meaning that we
# match on the outermost constructor of first arg only.
#
# This is the (admittedly trivial) example.
#
#reverse_helper(Nil, ?accum) == ?accum
#reverse_helper(Cons(?x, ?xs), ?accum) == reverse_helper(?xs, Cons(?x, ?accum))
#reverse(?xs) == reverse_helper(?xs, Nil)
#
# Second, recode it using "concatenative double dispatch", meaning that we
# mangle the pattern matching into the name so that the left hand side args
# no longer have any structure.
#
#_reverse_helper_nil(?accum) == ?accum
#_reverse_helper_cons(?x, ?xs, ?accum) == reverse_helper(?xs, Cons(?x, ?accum))
#reverse_helper(?xs, ?accum) == eat_?xs(_reverse_helper, ?accum)
#reverse(?xs) == reverse_helper(?xs, Nil)
#
# Third, try to recode in m4, and try not to make TOO much of a mess.
# Conventions:
#  Try for a block of definitions defining a new domain-specific language,
#  followed by use of that domain-specific language without contaminating it
#  with m4isms.
#  Things starting with underscore should not be invoked from a far location.
#  Things starting with capitals are inactive (for debug printing).
#  Things starting with a lowercase letter are part of the dsl.
#  Things that might be local functions in Lisp are suffixed by helper
changequote
changequote([,])

define([_gensym],[0])
define([gensym],[_gensym[]define([_gensym],incr(_gensym))])

define([eat_Nil],[$1_nil(shift($@))])
define([nil],[Nil])

define([_cons],[define([eat_Pair]$1,[$][1_cons($2,$3,shift($][@))])Pair$1])
define([cons],[_cons(gensym,$1,$2)])

define([_reverse_helper_nil],[$1])
define([_reverse_helper_cons],[reverse_helper($2,cons($1,$3))])
define([reverse_helper],[eat_$1([_reverse_helper],[$2])])

define([reverse],[reverse_helper([$1],[nil])])

define([_car_nil],[errprint([car of nil taken
])m4exit(1)])
define([_car_cons],[$1])
define([car],[eat_$1([_car])])

define([_cdr_nil],[errprint([cdr of nil taken
])m4exit(1)])
define([_cdr_cons],[$2])
define([cdr],[eat_$1([_cdr])])

define([_printlist_nil],[Nil])
define([_printlist_cons],[Cons($1,printlist($2))])
define([printlist],[eat_$1([_printlist])])

divert[]dnl
printlist(reverse(cons(1,cons(2,cons(3,nil)))))
printlist(reverse(cons(1,cons(2,cons(3,nil)))))
#car(nil)
#cdr(nil)
car(reverse(cons(1,cons(2,nil))))
printlist(cdr(reverse(cons(1,cons(2,nil)))))
