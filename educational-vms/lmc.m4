divert(-1)
changequote([,])
# This is an implementation of the "Little Man Computer" in m4.
# The LMC is a simple decimal-based accumulator machine used
# for educational purposes by Stuart Madnick in 1965.

# memory is a global array, called the "mailboxes" in the LMC
define([lookup], [lookup_$1])
define([store], [define([lookup_$1], [$2])])

# the program counter and accumulator both start at zero
define([run], [fetch(0, 0)])

# first fetch the instruction, and increment the program counter
define([fetch], [decode(lookup($1), incr($1), $2)])

# second decode the instruction into op and arg
define([decode], [dispatch(eval($1 / 100), eval($1 % 100), $2, $3)])

# third, we do a dispatch on the op, to one of the executes
define([dispatch], [execute_$1($2, $3, $4)])

# 1xx is ADD - add to the accum a value from memory
define([execute_1], [fetch($2, eval($3 + lookup($1)))])

# 2xx is SUB - subtract from accum a value from memory
define([execute_2], [fetch($2, eval($3 - lookup($1)))])

# 3xx is STORE - store from the accum into the memory
define([execute_3], [store($1, $3)fetch($2, $3)])

# 5xx is LOAD - load from the memory into the accum
define([execute_5], [fetch($2, lookup($1))])

# 6xx is branch unconditionally
define([execute_6], [fetch($1, $3)])

# 7xx is branch-if-zero
define([execute_7], [ifelse($3, 0, [fetch($1, $3)], [fetch($2, $3)])])

# 8xx is branch-if-nonnegative
define([execute_8], [ifelse(eval($3 < 0, 0, [fetch($2, $3)], [fetch($1, $3)])])

# 9xx are primitives; we dispatch to some specific primitive
define([execute_9], [primitive_$1($2, $3)])

# primitive 1 is ought to be read - but I'm not sure how to do it in m4

# primitive 2 is write
define([primitive_2], [$2
fetch($1, $2)])

# primitive 3 is halt
define([primitive_3], [])



# now an actual program

store([0], [599]) # load mem[99] into accum
# top of loop:
store([1], [902]) # write accum
store([2], [705]) # if accum is zero, goto done
store([3], [298]) # subtract mem[98] into accum
store([4], [601]) # branch to top of loop
# done:
store([5], [903]) # halt

# data
store([98], [1])
store([99], [10])



divert[]dnl
run()dnl
