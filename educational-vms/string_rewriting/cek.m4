divert(-1)
# This is an implementation of the CEK machine,
# loosely following Felleisen and Flatt's unpublished lecture notes:
# "Programming Languages and Lambda Calculi",
# specifically, page 80, the summary of the CEK machine.

# There's no sophisticated name-handling, and so there are certainly name-capturing bugs.

# In order to not make TOO much of a mess,
# some conventions:
# 1. If it starts with an underscore, don't invoke it from
# far away - just locally in that immediate paragraph.
# 2. Don't define macros starting with capitals.

# Some m4 preliminaries.
changequote
changequote([,])

# We use _gensym as a variable, holding the "next unused cell"
define([_gensym],[0])
# gensym returns the previous value of _gensym, and increments it.
define([gensym],[_gensym[]define([_gensym],incr(_gensym))])

# We use these "eat" functions as data structures;
# This is a bit like the visitor design pattern or double-dispatch in OO.
#
# The idea is that a constructor invocation like Cons(foo, bar),
# returns something like "Cons45". Then when you want to inspect that cell,
# you call eat_Cons45([inspector_function_prefix]),
# and eat_Cons45 calls you back at either inspector_function_prefix_nil
# or inspector_function_prefix_cons.

# environments bind names to closures - pairs of terms and environments
# env ::= Empty
define([eat_Empty], [$1_empty($2, $3)])
define([empty], [Empty])

# env ::= Extend(name, term, env, env)
define([_extend], [define([eat_Extend]$1, [$][1_extend($2, $3, $][2, $][3, $][4, $][5)])Extend$1])
define([extend], [_extend(gensym, $1, $2, $3, $4)])

# terms are simple lambda calculus terms
# term ::= Var(name)
define([_var], [define([eat_Var]$1, [$][1_var($2, $][2, $][3)])Var$1])
define([var], [_var(gensym, $1)])

# term ::= App(term, term)
define([_app], [define([eat_App]$1, [$][1_app($2, $3, $][2, $][3)])App$1])
define([app], [_app(gensym, $1, $2)])

# term ::= Lam(name, term)
define([_lam], [define([eat_Lam]$1, [$][1_app($2, $3, $][2, $][3)])Lam$1])
define([lam], [_lam(gensym, $1, $2)])

# this continuation datastructure is nonobvious,
# but it describes suspended "to-do" tasks, keeping enough info
# that we can do what we intended to do.
# cont ::= Return
define([eat_Return], [$1_return($2, $3)])
define([return], [Return])

# cont ::= Arg(term, env, cont)
define([_arg], [define([eat_Arg]$1, [$][1_arg($2, $3, $4, $][2, $][3)])Arg$1])
define([arg], [_arg(gensym, $1, $2, $3)])

# cont ::= Fun(name, term, env, cont)
define([_fun], [define([eat_Fun]$1, [$][1_fun($2, $3, $4, $5, $][2, $][3)])Fun$1])
define([fun], [_fun(gensym, $1, $2, $3, $4)])

# to eval a variable, look it up in the environment.
#cek(Var(?x), ?e, ?k) == lookup_and_run(?e, ?x, ?k)
define([_cek_var], [lookup_and_run($2, $1, $3)])
# to eval an application, eval the left term, and schedule the right for later.
#cek(App(?m, ?n), ?e, ?k) == cek(?m, ?e, Arg(?n, ?e, ?k))
define([_cek_app], [cek($1, $3, arg($2, $3, $4))])
# to eval a "value" - effectively, a lambda for now - look at the continuation.
#cek(Lam(?x, ?m), ?e, ?k) == cek_lam_helper(?k, ?x, ?m, ?e)
define([_cek_lam], [cek_lam_helper($4, $1, $2, $3)])
# dispatch
define([cek], [eat_$1([_cek], $2, $3)])

#lookup_and_run(Empty, ?x, ?k) == ARRGH!
define([_lookup_and_run_empty], [errprint([Could not find a definition of $1.
]m4exit(1))])
#lookup_and_run(Extend(?x0, ?v, ?ve, ?e), ?x1, ?k) == if ?x0 = ?x1 then cek(?v, ?ve, ?k) else lookup_and_run(?e, ?x1, ?k)
define([_lookup_and_run_extend], [ifelse($1, $5, [cek($2, $3, $6)], [lookup_and_run($4, $5, $6)])])
# dispatch
define([lookup_and_run], [eat_$1([_lookup_and_run], $2, $3)])

#cek_lam_helper(Return, ?x, ?m, ?e) == Lam(?x, ?m)
define([_cek_lam_helper_return], [lam($1, $2)])
#cek_lam_helper(Fun(?x, ?m, ?eprime, ?k), ?foo, ?bar, ?e) == cek(?m, Extend(?x, Lam(?foo, ?bar), ?e, ?eprime), ?k)
define([_cek_lam_helper_fun], [cek($2, extend($1, lam($5, $6), $7, $3), $4)])
#cek_lam_helper(Arg(?n, ?eprime, ?k), ?foo, ?bar, ?e) == cek(?n, ?eprime, Fun(?foo, ?bar, ?e, ?k))
define([_cek_lam_helper_arg], [cek($1, $2, fun($4, $5, $6, $3))])
# dispatch
define([cek_lam_helper], [eat_$1([_cek_lam_helper], $2, $3, $4)])

#eval_cek(?m) == cek(?m, Empty, Return)
define([eval_cek], [cek($1, empty, return)])

#S == Lam(x, Lam(y, Lam(z, App(App(Var(x), Var(z)), App(Var(y), Var(z))))))
define(s, lam(x, lam(y, lam(z, app(app(var(x), var(z)), app(var(y), var(z)))))))
#K == Lam(x, Lam(y, Var(x)))
define(k, lam(x, lam(y, var(x))))
#I == Lam(x, Var(x))
define(i, lam(x, var(x)))

divert[]dnl

i
eval_cek(i)
k
eval_cek(k)
s
eval_cek(s)
#eval_cek(app(i, i))
#eval_cek(app(app(k, i), s))

# does not terminate
#eval_cek(app(lam(x, app(var(x), var(x))), lam(x, app(var(x), var(x)))))

