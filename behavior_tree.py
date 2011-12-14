#!/usr/bin/python
#
# An attempt at implementing behavior trees,
# as described by Damian Isla, Chris Hecker,
# Alex Champandard, and Bjoern Knafla.
#
# I'm incompetent in Python, so this probably
# looks very awkward. Sorry.

class Done:
  def done(self):
    return True

  def push_left(self):
    return self

  def push_right(self):
    return self

Failure = Done()
Success = Done()

class Running:
  def done(self):
    return False

  def push_left(self):
    return Left(self)

  def push_right(self):
    return Right(self)

# I don't know how to make a single object
# that has some unique behavior in Python.
# This underscore-prefixing is disgusting,
# but it would work if I can't figure out
# something else.
class _Op:
  def __init__(self, killer, identity, is_reactive):
    self.killer = killer
    self.identity = identity
    self.is_reactive = is_reactive

  def short_cuts_on(self, result):
    return result == self.killer

  def id(self):
    return self.identity

  def reactive(self):
    return self.is_reactive

SequentialAnd = _Op(Failure, Success, False)
ReactiveAnd = _Op(Failure, Success, True)
SequentialOr = _Op(Success, Failure, False)
ReactiveOr = _Op(Success, Failure, True)

class Starter:
  def run(self, tree):
    return tree.start()

class PathFollower:
  def __init__(self, path):
    self.path = path

  def run(self, tree):
    return self.path.step(tree)

class Branch:
  def __init__(self, left, op, right):
    self.left = left
    self.op = op
    self.right = right

  def eval(self, l_generator, r_generator):
    x = l_generator.run(self.left)
    if self.op.short_cuts_on(x):
      return x
    elif not x.done():
      return x.push_left()
    elif x == self.op.id():
      return r_generator.run(self.right).push_right()
    else:
      assert False, 'This should not happen'

  def start(self):
    return self.eval(Starter(), Starter())

class Leaf:
  def __init__(self, x):
    self.x = x

  def start(self):
    return self.x

class Delay:
  def __init__(self, delayed):
    self.delayed = delayed

  def start(self):
    return Resume()

  def resume(self):
    return self.delayed.start()

class Resume(Running):
  def step(self, delay):
    return delay.resume()

  def __str__(self):
    return ""

class Left(Running):
  def __init__(self, path):
    self.path = path

  def step(self, tree):
    return tree.eval(PathFollower(self.path), Starter())

  def __str__(self):
    return "l" + str(self.path)

class Right(Running):
  def __init__(self, path):
    self.path = path

  def step(self, tree):
    if tree.op.reactive():
      return tree.eval(Starter(), PathFollower(self.path))
    else:
      return self.path.step(tree.right).push_right()

  def __str__(self):
    return "r" + str(self.path)

def Prioritized(*items):
  return reduce(lambda l, r: Branch(l, ReactiveOr, r), items)

def Sequence(*items):
  return reduce(lambda l, r: Branch(l, SequentialAnd, r), items)

def Rule(test, action):
  return Branch(test, ReactiveAnd, action)


if __name__ == '__main__':
  import unittest

  class _Bad:
    pass # This class deliberately left blank

  Bad = _Bad()

  class TestBehaviorTree(unittest.TestCase):
    def test_sequential_one_step_cases(self):
      cases = [
        { 'l': Success, 'op': SequentialAnd, 'r':Success, 'is': Success },
        { 'l': Success, 'op': SequentialAnd, 'r':Failure, 'is': Failure },
        { 'l': Failure, 'op': SequentialAnd, 'r':Bad, 'is': Failure },
        { 'l': Failure, 'op': SequentialOr, 'r':Failure, 'is': Failure },
        { 'l': Failure, 'op': SequentialOr, 'r':Success, 'is': Success },
        { 'l': Success, 'op': SequentialOr, 'r':Bad, 'is': Success },
      ]
      for case in cases:
        tree = Branch(Leaf(case['l']), case['op'], Leaf(case['r']))
        self.assertEqual(tree.start(), case['is'])

    def test_two_step_cases(self):
      cases = [
        { 'l': Success, 'op': SequentialAnd, 'r':Success, 'is': Success },
        { 'l': Success, 'op': SequentialAnd, 'r':Failure, 'is': Failure },
        { 'l': Success, 'op': ReactiveAnd, 'r':Success, 'is': Success },
        { 'l': Success, 'op': ReactiveAnd, 'r':Failure, 'is': Failure },
        { 'l': Failure, 'op': SequentialOr, 'r':Failure, 'is': Failure },
        { 'l': Failure, 'op': SequentialOr, 'r':Success, 'is': Success },
        { 'l': Failure, 'op': ReactiveOr, 'r':Failure, 'is': Failure },
        { 'l': Failure, 'op': ReactiveOr, 'r':Success, 'is': Success },
      ]
      for case in cases:
        tree = Delay(Branch(Leaf(case['l']), case['op'], Leaf(case['r'])))
        self.assertEqual(tree.start().step(tree), case['is'])

        tree = Branch(Delay(Leaf(case['l'])), case['op'], Leaf(case['r']))
        self.assertEqual(tree.start().step(tree), case['is'])

        tree = Branch(Leaf(case['l']), case['op'], Delay(Leaf(case['r'])))
        self.assertEqual(tree.start().step(tree), case['is'])

    def test_changing_cases(self):
      ops = [ SequentialAnd, ReactiveAnd, SequentialOr, ReactiveOr ]
      for op in ops:
        identity = op.id()
        killer = op.killer
        initially = Branch(Leaf(identity), op, Delay(Leaf(identity)))
        mind_state = initially.start()
        later = Branch(Leaf(killer), op, Delay(Leaf(identity)))
        result = mind_state.step(later)
        if op.reactive():
          self.assertEqual(result, killer)
        else:
          self.assertEqual(result, identity)

    # TODO: This is not really sufficient exercise of
    # Prioritized, Sequence, and Rule
    def test_tree_builders_insufficiently(self):
       # first the agent sees a path to the left
       clear_left = Leaf(Success)
       clear_ahead = Leaf(Failure)
       clear_right = Leaf(Failure)
       advance = Delay(Leaf(Success))
       turn_left = Delay(Leaf(Success))
       turn_right = Delay(Leaf(Success))
       policy = Prioritized(Rule(clear_ahead, advance),
                            Rule(clear_left, Sequence(turn_left, advance)),
                            Rule(clear_right, Sequence(turn_right, advance)))
       mind_state = policy.start()
       self.assertEqual(str(mind_state), "lrrl")
       # if nothing changes, then the agent goes on to advance
       mind_state = mind_state.step(policy)
       self.assertEqual(str(mind_state), "lrrr")
       # but if the agent now sees a path forward
       clear_ahead = Leaf(Success)
       policy = Prioritized(Rule(clear_ahead, advance),
                            Rule(clear_left, Sequence(turn_left, advance)),
                            Rule(clear_right, Sequence(turn_right, advance)))
       # then the agent instead follows the clear_ahead => advance rule
       mind_state = mind_state.step(policy)
       self.assertEqual(str(mind_state), "llr")

  unittest.main()

