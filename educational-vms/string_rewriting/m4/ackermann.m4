divert(-1)

define(ack,
  `ifelse(m, 0,
    `incr(n)',
    `ifelse(n, 0,
      `pushdef(`m',decr(m))pushdef(`n',1)ack`'popdef(`n')popdef(`m')',
      `pushdef(`n',decr(n))define(`temp',`ack')popdef(`n')pushdef(`m',decr(m))pushdef(`n',temp)ack`'popdef(`n')popdef(`m')')')')

divert`'dnl
traceon
pushdef(`m',3)dnl
pushdef(`n',3)dnl
ack(3,4)
popdef(`n')dnl
popdef(`m')dnl
