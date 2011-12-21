% -*- Prolog -*-
%
% An attempt at implementing behavior trees,
% as described by Damian Isla, Chris Hecker,
% Alex Champandard, and Bjoern Knafla.
%
% I'm incompetent in Prolog, so this probably
% looks very awkward. Sorry.

sequential_and(Op) :- !,
    Op = op(failure, success, false).

reactive_and(Op) :- !,
    Op = op(failure, success, true).

sequential_or(Op) :- !,
    Op = op(success, failure, false).

reactive_or(Op) :- !,
    Op = op(success, failure, false).

start(branch(L, Op, R), Out) :- !,
    eval(branch(L, Op, R), starter, starter, Out).
start(leaf(X), Out) :- !,
    Out = X.
start(delay(_Delayed), Out) :- !,
    Out = running(resume).

eval(branch(L, Op, R), LGenerator, RGenerator, Out) :- !,
    run(LGenerator, L, X),
    eval_rest(Op, X, RGenerator, R, Out).

eval_rest(Op, X, _RGenerator, _R, Out) :- short_cuts_on(Op, X), !,
    Out = X.
eval_rest(_Op, X, _RGenerator, _R, Out) :- not(done(X)), !,
    push_left(X, Out).
eval_rest(Op, X, RGenerator, R, Out) :- id(Op, X), !,
    run(RGenerator, R, Y),
    push_right(Y, Out).

run(starter, Tree, Out) :- !,
    start(Tree, Out).
run(pathfollower(Path), Tree, Out) :- !,
    step(Path, Tree, Out).


short_cuts_on(op(Killer, _Identity, _IsReactive), Out) :- !,
    Out = Killer.

done(failure).
done(success).

id(op(_Killer, Identity, _IsReactive), Out) :- !,
  Out = Identity.

push_left(running(Path), Out) :- !,
    Out = running(left(Path)).
push_left(X, Out) :- done(X), !,
    Out = X.

push_right(running(Path), Out) :- !,
    Out = running(right(Path)).
push_right(X, Out) :- done(X), !,
    Out = X.

step(running(Path), Tree, Out) :- !,
    step(Path, Tree, Out).
step(left(Path), Tree, Out) :- !,
    eval(Tree, pathfollower(Path), starter, Out).
step(right(Path), branch(L, Op, R), Out) :- is_reactive(Op), !, 
    eval(branch(L, Op, R), starter, pathfollower(Path), Out).
step(right(Path), branch(_L, Op, R), Out) :- not(is_reactive(Op)), !,
    step(Path, R, X), push_right(X, Out).
step(resume, DelayTree, Out) :- !,
    resume(DelayTree, Out).

resume(delay(Delayed), Out) :- !,
    start(Delayed, Out).

is_reactive(op(_Killer, _Identity, true)).

