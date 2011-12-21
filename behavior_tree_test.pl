% -*- Prolog -*-

% TODO: compress these tests to express generic properties

[behavior_tree].

sequential_and(Op), start(branch(leaf(success), Op, leaf(success)), success).

sequential_and(Op), start(branch(leaf(success), Op, leaf(failure)), failure).

sequential_and(Op), start(branch(leaf(failure), Op, bad), failure).

sequential_or(Op), start(branch(leaf(failure), Op, leaf(failure)), failure).

sequential_or(Op), start(branch(leaf(failure), Op, leaf(success)), success).

sequential_or(Op), start(branch(leaf(success), Op, bad), success).

sequential_and(Op), 
Tree = delay(branch(leaf(success), Op, leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

sequential_and(Op), 
Tree = branch(leaf(success), Op, delay(leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

sequential_and(Op), 
Tree = delay(branch(leaf(success), Op, leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

sequential_and(Op), 
Tree = branch(delay(leaf(success)), Op, leaf(failure)),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

sequential_and(Op), 
Tree = branch(leaf(success), Op, delay(leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

reactive_and(Op), 
Tree = delay(branch(leaf(success), Op, leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

reactive_and(Op), 
Tree = branch(delay(leaf(success)), Op, leaf(success)),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

reactive_and(Op), 
Tree = branch(leaf(success), Op, delay(leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

reactive_and(Op), 
Tree = delay(branch(leaf(success), Op, leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

reactive_and(Op), 
Tree = branch(delay(leaf(success)), Op, leaf(failure)),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

reactive_and(Op), 
Tree = branch(leaf(success), Op, delay(leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

sequential_or(Op), 
Tree = delay(branch(leaf(failure), Op, leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

sequential_or(Op), 
Tree = branch(leaf(failure), Op, delay(leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

sequential_or(Op), 
Tree = delay(branch(leaf(failure), Op, leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

sequential_or(Op), 
Tree = branch(delay(leaf(failure)), Op, leaf(success)),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

sequential_or(Op), 
Tree = branch(leaf(failure), Op, delay(leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

reactive_or(Op), 
Tree = delay(branch(leaf(failure), Op, leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

reactive_or(Op), 
Tree = branch(delay(leaf(failure)), Op, leaf(failure)),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

reactive_or(Op), 
Tree = branch(leaf(failure), Op, delay(leaf(failure))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = failure.

reactive_or(Op), 
Tree = delay(branch(leaf(failure), Op, leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

reactive_or(Op), 
Tree = branch(delay(leaf(failure)), Op, leaf(success)),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

reactive_or(Op), 
Tree = branch(leaf(failure), Op, delay(leaf(success))),
start(Tree, Mind),
step(Mind, Tree, Out),
Out = success.

sequential_and(Op),
id(Op, Identity),
short_cuts_on(Op, Killer),
Initially = branch(leaf(Identity), Op, delay(leaf(Identity))),
start(Initially, Mind),
Later = branch(leaf(Killer), Op, delay(leaf(Identity))),
step(Mind, Later, Result),
(is_reactive(Op) ->
    Result = Killer
;
    Result = Identity).

reactive_and(Op),
id(Op, Identity),
short_cuts_on(Op, Killer),
Initially = branch(leaf(Identity), Op, delay(leaf(Identity))),
start(Initially, Mind),
Later = branch(leaf(Killer), Op, delay(leaf(Identity))),
step(Mind, Later, Result),
(is_reactive(Op) ->
    Result = Killer
;
    Result = Identity).

sequential_or(Op),
id(Op, Identity),
short_cuts_on(Op, Killer),
Initially = branch(leaf(Identity), Op, delay(leaf(Identity))),
start(Initially, Mind),
Later = branch(leaf(Killer), Op, delay(leaf(Identity))),
step(Mind, Later, Result),
(is_reactive(Op) ->
    Result = Killer
;
    Result = Identity).

reactive_or(Op),
id(Op, Identity),
short_cuts_on(Op, Killer),
Initially = branch(leaf(Identity), Op, delay(leaf(Identity))),
start(Initially, Mind),
Later = branch(leaf(Killer), Op, delay(leaf(Identity))),
step(Mind, Later, Result),
(is_reactive(Op) ->
    Result = Killer
;
    Result = Identity).

%    # TODO: This is not really sufficient exercise of
%    # Prioritized, Sequence, and Rule
%    def test_tree_builders_insufficiently(self):
%       # first the agent sees a path to the left
%       clear_left = Leaf(Success)
%       clear_ahead = Leaf(Failure)
%       clear_right = Leaf(Failure)
%       advance = Delay(Leaf(Success))
%       turn_left = Delay(Leaf(Success))
%       turn_right = Delay(Leaf(Success))
%       policy = Prioritized(Rule(clear_ahead, advance),
%                            Rule(clear_left, Sequence(turn_left, advance)),
%                            Rule(clear_right, Sequence(turn_right, advance)))
%       mind_state = policy.start()
%       self.assertEqual(str(mind_state), "lrrl")
%       # if nothing changes, then the agent goes on to advance
%       mind_state = mind_state.step(policy)
%       self.assertEqual(str(mind_state), "lrrr")
%       # but if the agent now sees a path forward
%       clear_ahead = Leaf(Success)
%       policy = Prioritized(Rule(clear_ahead, advance),
%                            Rule(clear_left, Sequence(turn_left, advance)),
%                            Rule(clear_right, Sequence(turn_right, advance)))
%       # then the agent instead follows the clear_ahead => advance rule
%       mind_state = mind_state.step(policy)
%       self.assertEqual(str(mind_state), "llr")

%  unittest.main()

