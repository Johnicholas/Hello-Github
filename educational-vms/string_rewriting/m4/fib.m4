divert(-1)

define(`fib',
  `ifelse($1, 0,
    1,
    `ifelse($1, 1,
      1,
      `eval(fib(decr($1))+fib(decr(decr($1))))')')')

divert`'dnl
fib(10)
