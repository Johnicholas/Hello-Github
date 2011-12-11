#!/usr/bin/ruby -w
#
# This is a first stab at implementing behavior trees,
# as described by Damian Isla, Chris Hecker,
# Alex Champandard, and Bjoern Knafla.
#
# I'm incompetent in Ruby,
# so it probably looks very awkward.
# I apologize.
#
# A behavior has a start method that returns a result.
# Results are Success or Failure or Running.
# The outermost behavior in a real agent will
# always return a Running result.
# Running contains sufficient info to go back
# down the tree to whatever was taking so long,
# if you call its step method and pass in the tree.

module BehaviorTree
  class And
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def start
      @left.start.and(@right)
    end
  end

  class Or
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def start
      @left.start.or(@right)
    end
  end

  class Leaf
    def initialize(answer)
      @answer = answer
    end

    def start
      @answer.new
    end
  end

  class Success
    def and(right)
      right.start.and_right
    end

    def and_right
      self
    end

    def or(right_ignored)
      self
    end

    def or_right
      self
    end
  end

  class Failure
    def and(right_ignored)
      self
    end

    def and_right
      self
    end

    def or(right)
      right.start.or_right
    end

    def or_right
      self
    end
  end

  class Delay
    def initialize(tree)
      @tree = tree
    end

    def start
      Running.new
    end

    def resume
      @tree.start
    end
  end

  class Running
    def initialize
      @stack = [Proc.new do |tree, path|
        tree.resume
      end]
    end

    def and(right)
      @stack.push(Proc.new do |tree, path|
        path.step(tree.left).and(tree.right)
      end)
      self
    end

    def and_right
      @stack.push(Proc.new { |tree, path|
        path.step(tree.right).and_right
      })
      self
    end

    def or(right)
      @stack.push(Proc.new { |tree, path|
        path.step(tree.left).or(tree.right)
      })
      self
    end

    def or_right
      @stack.push(Proc.new { |tree, path|
        path.step(tree.right).or_right
      })
      self
    end

    def step(tree)
      first = @stack.pop
      first.call(tree, self)
    end
  end
end

if __FILE__ == $0
  require "test/unit"

  class TestBehaviorTree < Test::Unit::TestCase
    include BehaviorTree

    def test_and_can_succeed
      success_and_success = And.new(Leaf.new(Success), Leaf.new(Success))
      assert_equal Success, success_and_success.start.class
    end

    def test_and_can_fail
      success_and_failure = And.new(Leaf.new(Success), Leaf.new(Failure))
      assert_equal Failure, success_and_failure.start.class
    end

    class Erroneous
      # this class intentionally left blank
    end

    def test_and_can_short_circuit
      success_and_erroneous = And.new(Leaf.new(Failure), Erroneous.new)
      assert_equal Failure, success_and_erroneous.start.class
    end

    def test_or_can_fail
      failure_or_failure = Or.new(Leaf.new(Failure), Leaf.new(Failure))
      assert_equal Failure, failure_or_failure.start.class
    end

    def test_or_can_succeed
      failure_or_success = Or.new(Leaf.new(Failure), Leaf.new(Success))
      assert_equal Success, failure_or_success.start.class
    end

    def test_or_can_short_circuit
      success_or_erroneous = Or.new(Leaf.new(Success), Erroneous.new)
      assert_equal Success, success_or_erroneous.start.class
    end

    def test_and_can_delay_outside
      outside = Delay.new(And.new(Leaf.new(Success), Leaf.new(Success)))
      assert_equal Success, outside.start.step(outside).class
    end

    def test_and_can_delay_in_left_branch
      left = And.new(Delay.new(Leaf.new(Success)), Leaf.new(Success))
      assert_equal Success, left.start.step(left).class
    end

    def test_and_can_delay_in_right_branch
      right = And.new(Leaf.new(Success), Delay.new(Leaf.new(Success)))
      assert_equal Success, right.start.step(right).class
    end

    def test_or_can_delay_outside
      outside = Delay.new(Or.new(Leaf.new(Success), Leaf.new(Success)))
      assert_equal Success, outside.start.step(outside).class
    end

    def test_or_can_delay_in_left_branch
      left = Or.new(Delay.new(Leaf.new(Success)), Leaf.new(Success))
      assert_equal Success, left.start.step(left).class
    end

    def test_or_can_delay_in_right_branch
      right = Or.new(Leaf.new(Failure), Delay.new(Leaf.new(Success)))
      assert_equal Success, right.start.step(right).class
    end
  end
end
