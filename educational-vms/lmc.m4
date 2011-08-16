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

# first fetch the instruction.
define([fetch], [decode(lookup($1), $1, $2)])

# second decode the instruction into op and arg.
# by default, step to the next instruction.
define([decode], [dispatch(eval($1 / 100), eval($1 % 100), incr($2), $3)])

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
define([execute_8], [ifelse(eval($3 < 0), 1, [fetch($2, $3)], [fetch($1, $3)])])

# 9xx are primitives; we dispatch to some specific primitive
define([execute_9], [primitive_$1($2, $3)])

# primitive 1 is ought to be read - but I'm not sure how to do it in m4

# primitive 2 is write
define([primitive_2], [$2
fetch($1, $2)])

# primitive 3 is halt
define([primitive_3], [])

# some helpers for initializing the program in memory
# current keeps track of the current instruction
define([current_instruction], [0])
# add_instruction puts an instruction into memory, and increments current instruction
define([add_instruction], [store(current_instruction, $1)define([current_instruction], incr(current_instruction))]) 
# label allows defining convenient symbolic names for branch destinations
define([label], [define($1, current_instruction)])
# various operators can be written using symbolic names using add_instruction
define([add], [add_instruction(eval(100+$1))])
define([subtract], [add_instruction(eval(200+$1))])
define([store], [add_instruction(eval(300+$1))])
define([load], [add_instruction(eval(500+$1))])
define([branch], [add_instruction(eval(600+$1))])
define([branch_if_zero], [add_instruction(eval(700+$1))])
define([branch_if_nonnegative], [add_instruction(eval(800+$1))])
define([write], [add_instruction(902)])
define([halt], [add_instruction(903)])
# put initialized data at the other end of memory, counting in the other direction. 
define([data_counter], [99])
define([data], [store(data_counter, $1)define([data_counter], decr(data_counter))])
# allocate is like data, but defines a symbolic name for that location as well as an initial value
define([allocate], [define($1, data_counter)data($2)])

# now an actual program.
define([countdown], [
  allocate(counter, 10)
  allocate(step, 1)

  load(counter)
label(top_of_loop)
  subtract(step)
  branch_if_nonnegative(top_of_loop)
  halt() 
])

define([metacircular], [
  allocate(pc, 0) # the interpreted program counter, initially the interpreted zero
  allocate(a, 0) # the interpreted accumulator, also initially the interpreted
  allocate(LOAD, 500) # the constant 500, the code for load
  allocate(op, 0) # the op, once it's decoded; the zero is irrelevant
  allocate(arg, 0) # the arg, once it's decoded; the zero is irrelevant
  allocate(one_hundred, 100) # the constant 100, the spacing between opcodes
  allocate(temp, 0) # a temporary spot; the zero is irrelevant

label(top_of_loop)
  ## decode the current instruction into op and arg
  load(LOAD) # load the constant 500 into the accumulator
  add(pc) # add the program counter to the accumulator
  store(incr(current_instruction)) # store the accumulator into the upcoming instruction
  add_instruction(0) # this spot will be used to do the indirect-load
  # now that the accumulator has the instruction in it, we need to decide what to do
  # divide isn't a primitive - so we build it out of a loop.
label(top_of_divide_loop)
  # the test: instruction >= 100
  subtract(one_hundred)
  branch_if_nonnegative(eval(current_instruction+7))
  store(temp) # body 1
  load(op) # body 2
  add(one) # body 3
  store(op) # body 4
  load(temp) # body 5
  branch(top_of_divide_loop) # body 6
  

])



divert[]dnl
run()dnl
