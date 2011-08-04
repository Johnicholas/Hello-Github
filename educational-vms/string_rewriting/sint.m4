divert(-1)
changequote
changequote([,])
# This is an attempt at implementing Torben Mogensin's sint in m4.
# Conventions:
#  Try for a block of definitions defining a new domain-specific language,
#  followed by use of that domain-specific language without contaminating it
#  with m4isms.
#  Things starting with underscore should not be invoked from a far location.
#  Things starting with capitals are inactive (for debug printing).
#  Things starting with a lowercase letter are part of the dsl.
#  Things that might be local functions in Lisp are suffixed by "helper"
#
# run just delegates to apply, adding a Z and duplicating an argument
#run(?p, ?args) == apply(Z, ?p, ?args, ?p)
#
# apply is a little tricky, maybe I have to turn it into two mutually
# recursive bounce-and-swap things.
# original:
#apply(Z, Fun(?f, ?fs), ?args, ?p) == eval(?f, ?args, ?p)
#apply(S(?n), Fun(?f, ?fs), ?args, ?p) == apply(?n, ?fs, ?args, ?p)
# modified:
#apply_Z(Fun(?f, ?fs), ?args, ?p) == eval(?f, ?args, ?p)
#apply_Z(Fn, ?args, ?p) == AGGGH!
#apply(Z, ?funs, ?args, ?p) == apply_Z(?funs, ?args, ?p)
#apply_S(Fun(?f, ?fs), ?n, ?args, ?p) == apply(?n, ?fs, ?args, ?p)
#apply_S(Fn, ?n, ?args, ?p) == AGGGH! 
#apply(S(?n), ?funs, ?args, ?p) == apply_S(?funs, ?n, ?args, ?p)
#
# eval is straightforwardly object-oriented
#eval(Var(?n), ?vs, ?p) == lookup(?n, ?vs)
#eval(Fap(?n, ?es), ?vs, ?p) == apply(?n, ?p, evallist(?es, ?vs, ?p), ?p)
#eval(Cap(?n, ?es), ?vs, ?p) == Con(?n, evallist(?es, ?vs, ?p))
#eval(Case(?e, ?es), ?vs, ?p) == evalcasehelper(eval(?e, ?vs, ?p), ?es, ?vs, ?p)
#evalcasehelper(Con(?n, ?vs1), ?es, ?vs, ?p) == eval(select(?es, ?n), append(?vs1, ?vs), ?p)
#
# evallist is straightforwardly object-oriented
#evallist(En, ?vs, ?p) == Un
#evallist(Ec(?e, ?es), ?vs, ?p) == Uc(eval(?e, ?vs, ?p), evallist(?es, ?vs, ?p))
#
# select should probably be turned into two bouncy things.
# Original:
#select(Ec(?e, ?es), Z) == ?e
#select(Ec(?e, ?es), S(?n)) == select(?es, ?n)
# Modified:
#select_Ec(Z, ?e, ?es) == ?e
#select_Ec(S(?n), ?e, ?es) == select(?es, ?n)
#select(Ec(?e, ?es), ?n) == select_Ec(?n, ?e, ?es)
#select(En, ?n) == AGGGH!
#
# lookup is straightforwardly object-oriented
#lookup(Z, ?vs) == hd(?vs)
#lookup(S(?n), ?vs) == lookup(?n, tl(?vs))
#
# append is straightforwardly object-oriented
#append(Un, ?b) == ?b
#append(Uc(?v, ?a), ?b) == Uc(?v, append(?a, ?b))
#
#hd(Uc(?v, ?vs)) == ?v
#hd(Un) == AGGGH!
#tl(Uc(?v, ?vs)) == ?vs
#tl(Un) == AGGGH!
#
# After that minor redesign of the req implementation, we're following this
# example of how to do this ADT/concatenative double dispatch idiom in m4.
#
#reverse_helper(Nil, ?accum) == ?accum
#reverse_helper(Cons(?x, ?xs), ?accum) == reverse_helper(?xs, Cons(?x, ?accum))
#reverse(?xs) == reverse_helper(?xs, Nil)
#
# becomes:
#
#define([_gensym],[0])
#define([gensym],[_gensym[]define([_gensym],incr(_gensym))])
#define([eat_Nil],[$1_nil(shift($@))])
#define([nil],[Nil])
#define([_cons],[define([eat_Pair]$1,[$][1_cons($2,$3,shift($][@))])Pair$1])
#define([cons],[_cons(gensym,$1,$2)])
#define([_reverse_helper_nil],[$1])
#define([_reverse_helper_cons],[reverse_helper($2,cons($1,$3))])
#define([reverse_helper],[eat_$1([_reverse_helper],[$2])])
#define([reverse],[reverse_helper([$1],[nil])])

define([_gensym],[0])
define([gensym],[_gensym[]define([_gensym],incr(_gensym))])

# One paragraph for each ADT constructor

# num ::= Z | S( num )
define([eat_Z],[$1_z(shift($@))])
define([z],[Z])
define([_s],[define([eat_S]$1,[$][1_s($2,shift($][@))])S$1])
define([s],[_s(gensym,$1)])

# univ ::= Con( num, ulist )
define([_con],[define([eat_Con]$1,[$][1_con($2,$3,shift($][@))])Con$1])
define([con],[_con(gensym,$1,$2)])

# ulist ::= Un | Uc( univ, ulist )
define([eat_Un],[$1_un(shift($@))])
define([un],[Un])
define([_uc],[define([eat_Uc]$1,[$][1_uc($2,$3,shift($][@))])Uc$1])
define([uc],[_uc(gensym,$1,$2)])

# funs ::= Fn | Fun( exp, funs )
define([eat_Fn],[$1_fn(shift($@))])
define([fn],[Fn])
define([_fun],[define([eat_Fun]$1,[$][1_fun($2,$3,shift($][@))])Fun$1])
define([fun],[_fun(gensym,$1,$2)])

# exp ::= Var( num )
#       | Fap( num, elist )
#       | Cap( num, elist )
#       | Case( exp, elist )
define([_var],[define([eat_Var]$1,[$][1_var($2,shift($][@))])Var$1])
define([var],[_var(gensym,$1)])
define([_fap],[define([eat_Fap]$1,[$][1_fap($2,$3,shift($][@))])Fap$1])
define([fap],[_fap(gensym,$1,$2)])
define([_cap],[define([eat_Cap]$1,[$][1_cap($2,$3,shift($][@))])Cap$1])
define([cap],[_cap(gensym,$1,$2)])
define([_case],[define([eat_Case]$1,[$][1_case($2,$3,shift($][@))])Case$1])
define([case],[_case(gensym,$1,$2)])

# elist ::= En | Ec( exp, elist )
define([eat_En],[$1_en(shift($@))])
define([en],[En])
define([_ec],[define([eat_Ec]$1,[$][1_ec($2,$3,shift($][@))])Ec$1])
define([ec],[_ec(gensym,$1,$2)])

# One paragraph for each function

#run(?p, ?args) == apply(Z, ?p, ?args, ?p)
define([run],[apply([Z],[$1],[$2],[$1])])

#apply_Z(Fun(?f, ?fs), ?args, ?p) == mogeval(?f, ?args, ?p)
#apply_Z(Fn, ?args, ?p) == AGGGH!
define([_apply_Z_fun],[mogeval([$1],[$3],[$4])])
define([_apply_Z_fn],[errprint([attempted to apply a nonexistent function
])m4exit(1)])
define([apply_Z],[eat_$1([_apply_Z],[$2],[$3])])

#apply_S(Fun(?f, ?fs), ?n, ?args, ?p) == apply(?n, ?fs, ?args, ?p)
#apply_S(Fn, ?n, ?args, ?p) == AGGGH! 
define([_apply_S_fun],[apply([$3],[$2],[$4],[$5])])
define([_apply_S_fn],[errprint([attempted to apply a nonexistent function
])m4exit(1)])
define([apply_S],[eat_$1([_apply_S],[$2],[$3],[$4])])

#apply(Z, ?funs, ?args, ?p) == apply_Z(?funs, ?args, ?p)
#apply(S(?n), ?funs, ?args, ?p) == apply_S(?funs, ?n, ?args, ?p)
define([_apply_z],[apply_Z([$1],[$2],[$3])])
define([_apply_s],[apply_S([$2],[$1],[$3],[$4])])
define([apply],[eat_$1([_apply],[$2],[$3],[$4])])

#mogeval(Var(?n), ?vs, ?p) == lookup(?n, ?vs)
#mogeval(Fap(?n, ?es), ?vs, ?p) == apply(?n, ?p, evallist(?es, ?vs, ?p), ?p)
#mogeval(Cap(?n, ?es), ?vs, ?p) == Con(?n, evallist(?es, ?vs, ?p))
#mogeval(Case(?e, ?es), ?vs, ?p) == evalcasehelper(mogeval(?e, ?vs, ?p), ?es, ?vs, ?p)
define([_mogeval_var],[lookup([$1],[$2])])
define([_mogeval_fap],[apply([$1],[$4],evallist([$2],[$3],[$4]),[$4])])
define([_mogeval_cap],[con([$1],evallist([$2],[$3],[$4]))])
define([_mogeval_case],[evalcasehelper(mogeval([$1],[$3],[$4]),[$2],[$3],[$4])])
define([mogeval],[eat_$1([_mogeval],[$2],[$3])])

#evalcasehelper(Con(?n, ?vs1), ?es, ?vs, ?p)
# == mogeval(select(?es, ?n), append(?vs1, ?vs), ?p)
define([_evalcasehelper_con],[mogeval(select([$3],[$1]),append([$2],[$4]),$5)])
define([evalcasehelper],[eat_$1([_evalcasehelper],[$2],[$3],[$4])])

#evallist(En, ?vs, ?p) == Un
#evallist(Ec(?e, ?es), ?vs, ?p) == Uc(mogeval(?e, ?vs, ?p), evallist(?es, ?vs, ?p))
define([_evallist_en],[un])
define([_evallist_ec],[uc(mogeval([$1],[$3],[$4]),evallist([$2],[$3],[$4]))])
define([evallist],[eat_$1([_evallist],[$2],[$3])])

#select_Ec(Z, ?e, ?es) == ?e
#select_Ec(S(?n), ?e, ?es) == select(?es, ?n)
define([_select_Ec_z],[$1])
define([_select_Ec_s],[select([$3],[$1])])
define([select_Ec],[eat_$1([_select_Ec],[$2],[$3])])

#select(En, ?n) == AGGGH!
#select(Ec(?e, ?es), ?n) == select_Ec(?n, ?e, ?es)
define([_select_en],[errprint([attempted to select a nonexistent case branch
])m4exit(1)])
define([_select_ec],[select_Ec([$3],[$1],[$2])])
define([select],[eat_$1([_select],[$2])])

#lookup(Z, ?vs) == hd(?vs)
#lookup(S(?n), ?vs) == lookup(?n, tl(?vs))
define([_lookup_z],[hd([$1])])
define([_lookup_s],[lookup([$1],tl([$2]))])
define([lookup],[eat_$1([_lookup],[$2])])

#append(Un, ?b) == ?b
#append(Uc(?v, ?a), ?b) == Uc(?v, append(?a, ?b))
define([_append_un],[$1])
define([_append_uc],[uc([$1],append([$2],[$3]))])
define([append],[eat_$1([_append],[$2],[$3])])

#hd(Un) == AGGGH!
#hd(Uc(?v, ?vs)) == ?v
define([_hd_un],[errprint([attempted to take the head of an empty list
])m4exit(1)])
define([_hd_uc],[$1])
define([hd],[eat_$1([_hd])])
define([_hd_z],[errprint([attempted to take the head of a nonlist!
])m4exit(1)])


#tl(Un) == AGGGH!
#tl(Uc(?v, ?vs)) == ?vs
define([_tl_un],[errprint([attempted to take the tail of an empty list
])m4exit(1)])
define([_tl_uc],[$2])
define([tl],[eat_$1([_tl])])

define([_print_num_z],[Z])
define([_print_num_s],[S(print_num([$1]))])
define([print_num],[eat_$1([_print_num])])
define([_print_ulist_un],[Un])
define([_print_ulist_uc],[Uc(print_univ([$1],[$2]))])
define([print_ulist],[eat_$1([_print_ulist],[$2],[$3])])
define([_print_univ_con],[Con(print_num([$1]),print_ulist([$2]))])
define([print_univ],[eat_$1([_print_univ],[$2],[$3])])

define([sint],[fun(fap(s(z),
ec(cap(z,en),
ec(var(z),
ec(var(s(z)),
ec(var(z),
en))))),
fun(case(var(s(z)),
ec(cap(z,
ec(cap(z,en),
ec(cap(z,en),
en))),
ec(case(var(s(s(z))),
ec(fap(s(s(z)),
ec(var(z),
ec(var(s(s(s(s(z))))),
ec(var(s(s(s(s(s(z)))))),
en)))),
ec(fap(s(z),
ec(var(z),
ec(var(s(s(z))),
ec(var(s(s(s(s(s(z)))))),
ec(var(s(s(s(s(s(s(z))))))),
en))))),
en))),
en))),
fun(case(var(z),
ec(fap(s(s(s(s(z)))),
ec(var(z),
ec(var(s(s(z))),
en))),
ec(fap(s(z),
ec(var(z),
ec(var(s(s(s(s(z))))),
ec(fap(s(s(s(z))),
ec(var(s(z)),
ec(var(s(s(s(z)))),
ec(var(s(s(s(s(z))))),
en)))),
ec(var(s(s(s(s(z))))),
en))))),
ec(cap(z,
ec(var(z),
ec(fap(s(s(s(z))),
ec(var(s(z)),
ec(var(s(s(s(z)))),
ec(var(s(s(s(s(z))))),
en)))),
en))),
ec(case(fap(s(s(z)),
ec(var(z),
ec(var(s(s(s(z)))),
ec(var(s(s(s(s(z))))),
en)))),
ec(fap(s(s(z)),
ec(fap(s(s(s(s(s(z))))),
ec(var(z),
ec(var(s(s(s(z)))),
en))),
ec(fap(s(s(s(s(s(s(z)))))),
ec(var(s(z)),
ec(var(s(s(s(s(s(z)))))),
en))),
ec(var(s(s(s(s(s(s(z))))))),
en)))),
en)),
en))))),
fun(case(var(z),
ec(cap(z,en),
ec(cap(s(z),
ec(fap(s(s(z)),
ec(var(z),
ec(var(s(s(s(z)))),
ec(var(s(s(s(s(z))))),
en)))),
ec(fap(s(s(s(z))),
ec(var(s(z)),
ec(var(s(s(s(z)))),
ec(var(s(s(s(s(z))))),
en)))),
en))),
en))),
fun(case(var(z),
ec(fap(s(s(s(s(s(s(s(z))))))),
ec(var(s(z)),
en)),
ec(fap(s(s(s(s(z)))),
ec(var(z),
ec(fap(s(s(s(s(s(s(s(s(z)))))))),
ec(var(s(s(z))),
en)),
en))),
en))),
fun(case(var(s(z)),
ec(cap(s(s(z)),
ec(cap(z,en),
ec(cap(z,en),
en))),
ec(case(var(s(s(z))),
ec(var(z),
ec(fap(s(s(s(s(s(z))))),
ec(var(z),
ec(var(s(s(z))),
en))),
en))),
en))),
fun(case(var(z),
ec(var(s(z)),
ec(cap(s(z),
ec(var(z),
ec(fap(s(s(s(s(s(s(z)))))),
ec(var(s(z)),
ec(var(s(s(s(z)))),
en))),
en))),
en))),
fun(case(var(z),
ec(cap(z,
ec(cap(z,en),
ec(cap(z,en),
en))),
ec(var(z),
en))),
fun(case(var(z),
ec(cap(z,en),
ec(var(s(z)),
en))),
fn)))))))))])

define([fibprog], [fun(case(var(z),
ec(cap(s(z),ec(cap(z,en),en)),
ec(case(var(z),
ec(cap(s(z),ec(cap(z,en),en)),
ec(fap(s(z),
ec(fap(z,ec(var(s(z)),en)),
ec(fap(z,ec(var(z),en)),
en))),
en))),
en))),
fun(case(var(z),
ec(var(s(z)),
ec(cap(s(z),
ec(fap(s(z),
ec(var(z),
ec(var(s(s(z))),
en))),
en)),
en))),
fn))])

# value construction helpers
define([vsingleton],[uc([$1],un)])
define([vz],[con(z,un)])
define([vs],[con(s(z),vsingleton([$1]))])

divert[]dnl

print_univ(run(fibprog,vsingleton(vz)))
print_univ(run(fibprog,vsingleton(vs(vz))))
print_univ(run(fibprog,vsingleton(vs(vs(vz)))))
print_univ(run(fibprog,vsingleton(vs(vs(vs(vz))))))
print_univ(run(fibprog,vsingleton(vs(vs(vs(vs(vz)))))))
print_univ(run(fibprog,vsingleton(vs(vs(vs(vs(vs(vz))))))))
print_univ(run(fibprog,vsingleton(vs(vs(vs(vs(vs(vs(vz)))))))))


