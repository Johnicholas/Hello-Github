divert(-1)
define(`num_syms',0)
define(`gensym',`num_syms`'define(`num_syms',incr(num_syms))')
#define(`_iszero_S',`no')
#define(`_iszero_Z',`yes')
#define(`iszero',``$1'(`_iszero')')
#define(`num_0',`$1_Z')
#define(`zero',`ifelse($1,`consume',`$2_Z',`

define(`zero',`ifelse($1,`',``zero'',`$1_Z')')
define(`one',`ifelse($1,`',``one'',`$1_S(zero)')')
define(`two',`ifelse($1,`',``two'',`$1_S(one)')')
define(`_iszero_Z',`yes')
define(`_iszero_S',`no')
define(`iszero',`$1(_iszero)')
define(`_iseven_Z',`yes')
define(`_iseven_S',`isodd($1)')
define(`iseven',`$1(_iseven)')
define(`_isodd_Z',`no')
define(`_isodd_S',`iseven($1)')
define(`isodd',`$1(_isodd)')

make_zero(?x) == define(`peano_?x',`ifelse($1,`',``?x'',`
define(`peano_Z',`make_zero(gensym)')

divert`'dnl
# gensym tests:
gensym
gensym
gensym
# zero tests:
iszero(zero)
iszero(one)
iszero(two)
# even tests:
iseven(zero)
iseven(one)
iseven(two)
# odd tests:
isodd(zero)
isodd(one)
isodd(two)

