#!/usr/bin/ruby -w
#
# My attempt at implementing behavior trees,
# as described by Damian Isla, Chirs Hecker,
# Alex Champandard, and Bjoern Knafla.
#
# I'm incompetent in ruby, so this probably
# looks very awkward. Sorry.

module BehaviorTree
  class Done
    def done?
      true
    end

    def push_left
      self
    end

    def push_right
      self
    end
  end

  Failure = Done.new

  Success = Done.new

  class Running
    def done?
      false
    end

    def push_left
      Left.new(self)
    end

    def push_right
      Right.new(self)
    end
  end

  BinaryAnd = Object.new

  def BinaryAnd.short_cuts_on(result)
    result == Failure
  end
  
  def BinaryAnd.id
    Success
  end

  def BinaryAnd.reactive?
    false
  end

  BinaryOr = Object.new

  def BinaryOr.short_cuts_on(result)
    result == Success
  end
  
  def BinaryOr.id
    Failure
  end
  
  def BinaryOr.reactive?
    false
  end

  ReactiveAnd = Object.new

  def ReactiveAnd.short_cuts_on(result)
    result == Failure
  end
  
  def ReactiveAnd.id
    Success
  end

  def ReactiveAnd.reactive?
    true
  end

  ReactiveOr = Object.new

  def ReactiveOr.short_cuts_on(result)
    result == Success
  end
  
  def ReactiveOr.id
    Failure
  end
  
  def ReactiveOr.reactive?
    true
  end

  class Branch
    attr_accessor :left, :op, :right

    def initialize(left, op, right)
      @left = left
      @op = op
      @right = right
    end

    def eval(left_lambda, right_lambda)
      x = left_lambda.call
      return x if @op.short_cuts_on(x)
      return x.push_left if not x.done?
      return right_lambda.call.push_right if x == @op.id
      raise "This should not happen"
    end

    def start
      eval(lambda { @left.start },
           lambda { @right.start })
    end
  end

  class Leaf
    attr_accessor :x

    def initialize(x)
      @x = x
    end

    def start
      @x
    end
  end

  class Delay
    def initialize(delayed)
      @delayed = delayed
    end

    def start
      Resume.new
    end

    def resume
      @delayed.start
    end
  end

  class Resume < Running
    def step(delay)
      delay.resume
    end

    def to_a
      []
    end
  end

  class Left < Running
    def initialize(path)
      @path = path
    end

    def step(tree)
      tree.eval(lambda { @path.step(tree.left) },
                lambda { tree.right.start })
    end

    def to_a
      @path.to_a.unshift(:left)
    end
  end

  class Right < Running
    def initialize(path)
      @path = path
    end

    def step(tree)
      if tree.op.reactive?
        tree.eval(lambda { tree.left.start },
                  lambda { @path.step(tree.right) })
      else
        @path.step(tree.right).push_right
      end
    end

    def to_a
      @path.to_a.unshift(:right)
    end
  end

  def Prioritized(items)
    items.inject { |l, r| Branch.new(l, ReactiveOr, r) }
  end

  def Sequence(items)
    items.inject { |l, r| Branch.new(l, BinaryAnd, r) }
  end

  def Rule(test, action)
    Branch.new(test, ReactiveAnd, action)
  end
end

if __FILE__ == $0
  require "test/unit"

  class TestBehaviorTree < Test::Unit::TestCase
    include BehaviorTree

    def test_success_and_success_is_success
      tree = Branch.new(Leaf.new(Success),
                        BinaryAnd,
                        Leaf.new(Success))
      assert_equal Success, tree.start
    end

    def test_success_and_failure_is_failure
      tree = Branch.new(Leaf.new(Success),
                        BinaryAnd,
                        Leaf.new(Success))
      assert_equal Success, tree.start
    end

    Erroneous = Object.new

    def test_and_can_short_cut
      tree = Branch.new(Leaf.new(Failure),
                        BinaryAnd,
                        Erroneous)
      assert_equal Failure, tree.start
    end

    def test_failure_or_failure_is_failure
      tree = Branch.new(Leaf.new(Failure),
                        BinaryOr,
                        Leaf.new(Failure))
      assert_equal Failure, tree.start
    end

    def test_failure_or_success_is_success
      tree = Branch.new(Leaf.new(Failure),
                        BinaryOr,
                        Leaf.new(Success))
      assert_equal Success, tree.start
    end

    def test_or_can_short_cut
      tree = Branch.new(Leaf.new(Success),
                        BinaryOr,
                        Erroneous)
      assert_equal Success, tree.start
    end

    def test_delay_outside_and
      tree = Delay.new(Branch.new(Leaf.new(BinaryAnd.id),
                                  BinaryAnd,
                                  Leaf.new(BinaryAnd.id)))
      assert_equal BinaryAnd.id, tree.start.step(tree)
    end
    
    def test_delay_in_left_and
      tree = Branch.new(Delay.new(Leaf.new(BinaryAnd.id)),
                        BinaryAnd,
                        Leaf.new(BinaryAnd.id))
      assert_equal BinaryAnd.id, tree.start.step(tree)
    end
    
    def test_delay_in_right_and
      tree = Branch.new(Leaf.new(BinaryAnd.id),
                        BinaryAnd,
                        Delay.new(Leaf.new(BinaryAnd.id)))
      assert_equal BinaryAnd.id, tree.start.step(tree)
    end
    
    def test_delay_outside_or
      tree = Delay.new(Branch.new(Leaf.new(BinaryOr.id),
                                  BinaryOr,
                                  Leaf.new(BinaryOr.id)))
      assert_equal BinaryOr.id, tree.start.step(tree)
    end
    
    def test_delay_in_left_or
      tree = Branch.new(Delay.new(Leaf.new(BinaryOr.id)),
                        BinaryOr,
                        Leaf.new(BinaryOr.id))
      assert_equal BinaryOr.id, tree.start.step(tree)
    end
    
    def test_delay_in_right_or
      tree = Branch.new(Leaf.new(BinaryOr.id),
                        BinaryOr,
                        Delay.new(Leaf.new(BinaryOr.id)))
      assert_equal BinaryOr.id, tree.start.step(tree)
    end
    
    def test_delay_outside_reactiveand
      tree = Delay.new(Branch.new(Leaf.new(ReactiveAnd.id),
                                  ReactiveAnd,
                                  Leaf.new(ReactiveAnd.id)))
      assert_equal ReactiveAnd.id, tree.start.step(tree)
    end
    
    def test_delay_in_left_reactiveand
      tree = Branch.new(Delay.new(Leaf.new(ReactiveAnd.id)),
                        ReactiveAnd,
                        Leaf.new(ReactiveAnd.id))
      assert_equal ReactiveAnd.id, tree.start.step(tree)
    end
    
    def test_delay_in_right_reactiveand
      tree = Branch.new(Leaf.new(ReactiveAnd.id),
                        ReactiveAnd,
                        Delay.new(Leaf.new(ReactiveAnd.id)))
      assert_equal ReactiveAnd.id, tree.start.step(tree)
    end
    
    def test_delay_outside_reactiveor
      tree = Delay.new(Branch.new(Leaf.new(ReactiveOr.id),
                                  ReactiveOr,
                                  Leaf.new(ReactiveOr.id)))
      assert_equal ReactiveOr.id, tree.start.step(tree)
    end
    
    def test_delay_in_left_reactiveor
      tree = Branch.new(Delay.new(Leaf.new(ReactiveOr.id)),
                        ReactiveOr,
                        Leaf.new(ReactiveOr.id))
      assert_equal ReactiveOr.id, tree.start.step(tree)
    end
    
    def test_delay_in_right_reactiveor
      tree = Branch.new(Leaf.new(ReactiveOr.id),
                        ReactiveOr,
                        Delay.new(Leaf.new(ReactiveOr.id)))
      assert_equal ReactiveOr.id, tree.start.step(tree)
    end
    
    def test_reactive_and_notices_a_novel_failure
      initial_success = Branch.new(Leaf.new(Success),
                                   ReactiveAnd,
                                   Delay.new(Leaf.new(Success)))
      later_fail = Branch.new(Leaf.new(Failure),
                              ReactiveAnd,
                              Delay.new(Leaf.new(Success)))
      assert_equal Failure, initial_success.start.step(later_fail)
    end
    
    def test_reactive_or_notices_a_novel_success
      initial_fail = Branch.new(Leaf.new(Failure),
                                ReactiveOr,
                                Delay.new(Leaf.new(Failure)))
      later_success = Branch.new(Leaf.new(Success),
                                 ReactiveOr,
                                 Delay.new(Leaf.new(Failure)))
      assert_equal Success, initial_fail.start.step(later_success)
    end

    def test_and_ignores_a_novel_failure
      initial_success = Branch.new(Leaf.new(Success),
                                   BinaryAnd,
                                   Delay.new(Leaf.new(Success)))
      later_fail = Branch.new(Leaf.new(Failure),
                              BinaryAnd,
                              Delay.new(Leaf.new(Success)))
      assert_equal Success, initial_success.start.step(later_fail)
    end

    def test_or_ignores_a_novel_success
      initial_fail = Branch.new(Leaf.new(Failure),
                                BinaryOr,
                                Delay.new(Leaf.new(Failure)))
      later_success = Branch.new(Leaf.new(Success),
                                 BinaryOr,
                                 Delay.new(Leaf.new(Failure)))
      assert_equal Failure, initial_fail.start.step(later_success)
    end

    def test_tree_builders_a_little
      # first the agent sees a path to the left
      clear_left = Leaf.new(Success)
      clear_ahead = Leaf.new(Failure)
      clear_right = Leaf.new(Failure)
      advance = Delay.new(Leaf.new(Success))
      turn_left = Delay.new(Leaf.new(Success))
      turn_right = Delay.new(Leaf.new(Success))
      policy = Prioritized([Rule(clear_ahead, advance),
                            Rule(clear_left, Sequence([ turn_left, advance ])),
                            Rule(clear_right, Sequence([ turn_right, advance ]))])
      mind_state = policy.start
      # and the agent decides to turn left
      assert_equal [:left, :right, :right, :left], mind_state.to_a
      # if nothing changes, then the agent goes on to advance
      mind_state = mind_state.step(policy)
      assert_equal [:left, :right, :right, :right], mind_state.to_a
      # but if the agent now sees a path forward
      clear_ahead = Leaf.new(Success)
      policy = Prioritized([Rule(clear_ahead, advance),
                            Rule(clear_left, Sequence([ turn_left, advance ])),
                            Rule(clear_right, Sequence([ turn_right, advance ]))])
      # then the agent instead follows the clear_ahead => advance rule 
      mind_state = mind_state.step(policy)
      assert_equal [:left, :left, :right], mind_state.to_a
    end

  end
end
