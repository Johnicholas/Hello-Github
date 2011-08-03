define(power,`ifelse($2, 0, 1, `eval($1*power($1,decr($2)))')')dnl
power(2,10)
