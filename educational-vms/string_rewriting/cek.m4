divert(-1)
changequote([,])
# The CEK machine, by Felleisen and Flatt, following Hayo Thielecke's slides
# mistakes and misunderstandings by Johnicholas

# This uses a particular idiom for abstract data types in m4.
# A constructor is implemented by:
# 1. using a counter to obtain a unique name
# 2. defining a way to dispatch or "eliminate" that name
# 3. returning the unique name
# 4. incrementing the counter

# we use just one counter for everything;
# we could use one counter per constructor instead
define([sym], [0])
define([next], [sym[]define([sym], incr(sym))])

# terms are unit, vars, applications, lambdas
# Aside: Unit is really a proxy for all kinds of primitives
# term ::= Unit | Var( num ) | App( term, term ) | Lam( term )
define([unit],[define([eat_Unit]sym,[$][1_unit(shift($][@))])Unit[]next])
define([var],[define([eat_Var]sym,[$][1_var($1,shift($][@))])Var[]next])
define([app],[define([eat_App]sym,[$][1_app($1,$2,shift($][@))])App[]next])
define([lam],[define([eat_Lam]sym,[$][1_lam($1,shift($][@))])Lam[]next])

# numbers are peano integers
# Aside: This is woefully inefficient, but I'm trying for simple, concrete,
# and working
# num ::= Z | S( num )
define([z],[define([eat_Z]sym,[$][1_z(shift($][@))])Z[]next])
define([s],[define([eat_S]sym,[$][1_s($1,shift($][@))])S[]next])

# values are unit and closures.
# Aside: remember when you're reading the closure,
# there's an invisible Lam() around the term,
# because all closures start with lambda
# val ::= Unit | Clos( term , env )
# Aside; Unit already has a constructor)
define([clos],[define([eat_Clos]sym,[$][1_clos($1,$2,shift($][@))])Clos[]next])

# environments can be constructed empty or extended
# env ::= Empty | Ext( val, env )
define([empty],[define([eat_Empty]sym,[$][1_empty(shift($][@))])Empty[]next])
define([ext],[define([eat_Ext]sym,[$][1_ext(shift($][@))])Ext[]next])

# The kinds of continuations are a bit unmotivated, 
# but these are what we'll turn out to need
# cont ::= Done | Arg( term, env, cont ) | Fun( val, cont )
define([done],[define([eat_Done]sym,[$][1_done(shift($][@))])Done[]next])
define([arg],[define([eat_Arg]sym,[$][1_arg($1,$2,$3,shift($][@))])Arg[]next])
define([fun],[define([eat_Fun]sym,[$][1_fun($1,$2,shift($][@))])Fun[]next])

# lookup is a helper function for environments
# in req, it would look like this:
# lookup(Z, ?env) == lookup_head(?env)
# lookup_head(Empty) == AAAGGGGGHH!
# lookup_head(Ext(?var, ?env)) == ?var
# lookup(S(?num), ?env) == lookup_tail(?env, ?num)
# lookup_tail(Empty, ?num) == AAAGGGGGHH!
# lookup_tail(Ext(?var, ?env), ?num) == lookup(?num, ?env)
define([lookup],[eat_$1([lookup],$2)])
define([lookup_z],[lookup_head($1)])
define([lookup_head],[eat_$1([lookup_head])])
define([lookup_head_empty],[errprint([Unable to find value for variable])])
define([lookup_head_ext],[$1])
define([lookup_s],[lookup_tail($2,$1)])
define([lookup_tail],[eat_$1([lookup_tail],$2)])
define([lookup_tail_empty],[errprint([Unable to find value for variable])])
define([lookup_tail_ext],[lookup($3, $2)])

# to eval a term, run the cek machine on it,
# starting with the empty environment and the done continuation.
# in req, it would look like this:
# cek_eval(?term) == cek(?term, Empty, Done)
define([cek_eval],[cek($1,empty,done)])

# the cek function first dispatches on the term, the first argument.
define([cek],[eat_$1([cek],$2,$3)])

# if the term is a variable, look it up in the env
# in req, it would look like this:
# cek(Var(?num), ?env, ?k) == cek(lookup(?num, ?env), ?env, ?k)
define([cek_var],[cek(lookup($1,$2),$2,$3)])

# if the term is an application, start with the left one,
# and schedule dealing with the right one for later
# in req, it would look like this:
# cek(App(?m1, ?m2), ?env, ?k) == cek(?m1, ?env, Arg(?m2, ?env, ?k))
define([cek_app],[cek($1,$3,arg($2,$3,$4))])

# if the term is a lambda, then form a closure.
# in req, it would look like this:
# cek(Lam(?m), ?env, ?k) == cek(Clos(?m, ?env), ?env, ?k)
define([cek_lam],[cek(clos($1,$2),$2,$3)])

# if there's a value in the first part of the triple, 
# whether that value is a closure or unit,
# then we can't go any further in that direction.
# in req, it would look like this:
# cek(Clos(?mx, ?ex), ?e1, ?k) == cek_val(?k, Clos(?mx, ?ex))
define([cek_clos],[cek_val($4,clos($1,$2))])
# in req, it would look like this:
# cek(Unit, ?e1, ?k) == cek_val(?k, Unit)
define([cek_unit],[cek_val($2,unit)])

# We dispatch on the continuation to decide what to do next.
define([cek_val],[eat_$1([cek_val],$2)])

# If we had planned on doing an argument next,
# schedule that we should come back to this,
# once we have the argument evalled.
# in req, it would look like this:
# cek_val(Arg(?m, ?e, ?k), ?w) == cek(?m, ?e, Fun(?w, ?k))
define([cek_val_arg],[cek($1,$2,fun($4,$3))])

# If we have both the left and the right halves of an
# application, then we can probably go inside.
# However, we need to check that the left half is
# a closure - it might be unit.
# cek_val(Fun(?w1, ?k), ?w2) == cek_val_fun_check(?w1, ?k, ?2)
define([cek_val_fun],[cek_val_fun_check($1,$2,$3)])

# dispatch
define([cek_val_fun_check],[eat_$1([cek_val_fun_check],$2,$3)])

# cek_val_fun_check(Unit,$k,$w) == AGGGHGH!
define([cek_val_fun_check_unit],[errprint([Attempted to apply unit to something])])
# cek_val_fun_check(Clos(?m,?e),?k,$w) == cek(?m, Ext(?w, ?e), ?k)
define([cek_val_fun_check_clos],[cek($1,ext($4,$2),$3)])

# If we don't have anything scheduled,
# then return the value.
# in req, it would look like this:
# cek_val(Done,?w) == ?w
define([cek_val_done],[$1])

divert
# tests

# test the identity function, applied to itself.
# should be a closure:
app(lam(var(z)),lam(var(z)))
cek_app(Lam2,Lam5,empty,done)

#eat(
#cek(app(lam(var(z)),lam(var(z))),empty,done)
#eat_(app

cek_eval(app(lam(var(z)),lam(var(z))))

# should be unit:
cek_eval(app(lam(var(z)),unit))

# define some combinators
define([s],lam(lam(lam(app(app(var(s(s(z))),var(z)),app(var(s(z)),var(z)))))))
define([k],lam(lam(var(s(z)))))
define([i],app(app(s,k),k))

# same tests using the sk version of the identity function
# should be a closure:
cek_eval(app(i,i))
# should be unit:
cek_eval(app(i,unit))

# does not terminate
#cek_eval(app(lam(app(var(z),var(z))),lam(app(var(z),var(z)))))
