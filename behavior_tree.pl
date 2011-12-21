% -*- Prolog -*-

sequential_and(Op) :-
    Op = op(failure, success, false).

reactive_and(Op) :-
    Op = op(failure, success, true).

sequential_or(Op) :-
    Op = op(success, failure, false).

reactive_or(Op) :-
    Op = op(success, failure, false).

start(branch(L, Op, R), Out) :-
    eval(branch(L, Op, R), starter, starter, Out).
start(leaf(X), Out) :-
    Out = X.
start(delay(_Delayed), Out) :-
    Out = running(resume).

% in Python, this would look something like:
%
%  def eval(self, l_generator, r_generator):
%    x = l_generator.run(self.left)
%    if self.op.short_cuts_on(x):
%      return x
%    elif not x.done():
%      return x.push_left()
%    elif x == self.op.id():
%      return r_generator.run(self.right).push_right()
%    else:
%      assert False, 'This should not happen'
eval(branch(L, Op, R), LGenerator, RGenerator, Out) :-
    run(LGenerator, L, X),
    (short_cuts_on(Op, X) ->
        Out = X
    ;(not(done(X)) ->
        push_left(X, Out)
    ;(id(Op, X) ->
        run(RGenerator, R, Y),
        push_right(Y, Out)
    ;
        fail
    ))).

run(starter, Tree, Out) :-
    start(Tree, Out).
run(pathfollower(Path), Tree, Out) :-
    step(Path, Tree, Out).


short_cuts_on(op(Killer, _Identity, _IsReactive), ToTest) :-
    ToTest = Killer.

done(failure).
done(success).

id(op(_Killer, Identity, _IsReactive), ToTest) :-
  ToTest = Identity.

push_left(running(Path), Out) :-
    Out = running(left(Path)).
push_left(X, Out) :-
    done(X),
    Out = X.

push_right(running(Path), Out) :-
    Out = running(right(Path)).
push_right(X, Out) :-
    done(X),
    Out = X.

step(running(Path), Tree, Out) :-
    step(Path, Tree, Out).
step(left(Path), Tree, Out) :-
    eval(Tree, pathfollower(Path), starter, Out).
step(right(Path), branch(L, Op, R), Out) :- 
    (is_reactive(Op) ->
        eval(branch(L, Op, R), starter, pathfollower(Path), Out)
    ;
        step(Path, R, X), push_right(X, Out)
    ).
step(resume, DelayTree, Out) :-
    resume(DelayTree, Out).

resume(delay(Delayed), Out) :-
    start(Delayed, Out).

is_reactive(op(_Killer, _Identity, true)).

