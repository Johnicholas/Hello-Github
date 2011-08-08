divert(-1)
# a global counter used for making things unique
define(`_gensym',`0')
define(`gensym',`_gensym`'define(`_gensym',incr(_gensym))')
# exp ::= Num( int )
define(`_num',`define(`eat_Num'$1,`$'`1_num($2,shift($'`@))')Num$1')
define(`num',`_num(gensym,$1)')
# exp ::= Plus( exp , exp )
define(`_plus',`define(`eat_Plus'$1,`$'`1_plus($2,$3,shift($'`@))')Plus$1')
define(`plus',`_plus(gensym,$1,$2)')
# define print_exp by cases
#   print_exp(Num(int)) => int
define(`_print_exp_num',`$1')
#   print_exp(Plus(exp1,exp2)) => print_exp(exp1)+print_exp(exp2)
define(`_print_exp_plus',`print_exp($1)+print_exp($2)')
# actually dispatch
define(`print_exp',`eat_$1(_print_exp)')
# define my_eval by cases
#   my_eval(Num(int)) => int
define(`_my_eval_num',`$1')
#   my_eval(Plus(exp1,exp2)) => eval(my_eval(exp1)+my_eval(exp2))
define(`_my_eval_plus',`eval(my_eval($1)+my_eval($2))')
# actually dispatch
define(`my_eval',`eat_$1(_my_eval)')
divert`'dnl
print_exp(plus(plus(num(1),num(2)),num(3)))
my_eval(plus(num(4),plus(num(5),num(6))))
