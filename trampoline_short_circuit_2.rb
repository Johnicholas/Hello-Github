#!/usr/bin/ruby -w
#
# This is an adjustment or refinement of short_circuit.rb
#
# We're trying to make a trampolined short-circuit evaluator.
# By trampolined, what I mean is that the evaluator's
# control flow repeatedly returns back to a single point,
# making it easy to interleave evaluation with doing
# something else, such as rendering a frame or evaling
# some other tree.
#
# TODO: AndHelper and OrHelper are still terrible names.
# They're more obviously closure-ish now, but
# I'm still using the verbose object-oriented syntax.
# I am not confident I can navigate Ruby's blocks,
# Procs, lambdas, and Methods. The idea is to
# do something that's straightforwardly
# analogous to something that you could do in other
# languages, such as Java, C++, Prolog or Soar.
#
# A behavior has an 'ev' (short for eval) method that
# takes an array of frames and returns a result.
#
# A frame has a continue method that
# takes a result and an array of frames and returns a result.
#
# A result... I'm not sure what its interface is.

module ShortCircuit

  class And
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def ev
      @left.ev.and(@right)
    end
  end

  class Or
    attr_reader :left, :right

    def initialize(left, right)
      @left = left
      @right = right
    end

    def ev
      @left.ev.or(@right)
    end
  end

  class Leaf
    def initialize(answer)
      @answer = answer
    end

    def ev
      @answer.new
    end
  end

  class Success
    def and(right)
      right.ev.and_right
    end

    def and_right
      Success.new      
    end

    def or(right_ignored)
      Success.new
    end

    def or_right
      Success.new
    end
  end

  class Failure
    def and(right_ignored)
      Failure.new
    end

    def and_right
      Failure.new
    end

    def or(right)
      right.ev.or_right
    end

    def or_right
      Failure.new
    end
  end

  class Delay
    def initialize(tree)
      @tree = tree
    end

    def ev
      Running.new
    end

    def resume
      @tree.ev
    end
  end

  class Running
    def initialize
      @stack = []
    end

    def and(right)
      @stack.unshift(AndLeft.new)
      self
    end

    def and_right
      @stack.unshift(AndRight.new)
      self
    end

    def or(right)
      @stack.unshift(OrLeft.new)
      self
    end

    def or_right
      @stack.unshift(OrRight.new)
      self
    end

    def step(tree)
      if @stack.empty?
        tree.resume
      else
        first = @stack.shift
        first.resume(tree, self)
      end
    end
  end

  class AndLeft
    def resume(tree, running)
      running.step(tree.left).and(tree.right)
    end
  end

  class AndRight
    def resume(tree, running)
      running.step(tree.right).and_right
    end
  end

  class OrLeft
    def resume(tree, running)
      running.step(tree.left).or(tree.right)
    end
  end

  class OrRight
    def resume(tree, running)
      running.step(tree.right).or_right
    end
  end
end

if __FILE__ == $0
  require "test/unit"

  class TestShortCircuit < Test::Unit::TestCase
    include ShortCircuit

    def test_and_can_succeed
      success_and_success = And.new(Leaf.new(Success), Leaf.new(Success))
      assert_equal Success, success_and_success.ev.class
    end

    def test_and_can_fail
      success_and_failure = And.new(Leaf.new(Success), Leaf.new(Failure))
      assert_equal Failure, success_and_failure.ev.class
    end

    class Erroneous
      # this class intentionally left blank
    end

    def test_and_can_short_circuit
      success_and_erroneous = And.new(Leaf.new(Failure), Erroneous.new)
      assert_equal Failure, success_and_erroneous.ev.class
    end

    def test_or_can_fail
      failure_or_failure = Or.new(Leaf.new(Failure), Leaf.new(Failure))
      assert_equal Failure, failure_or_failure.ev.class
    end

    def test_or_can_succeed
      failure_or_success = Or.new(Leaf.new(Failure), Leaf.new(Success))
      assert_equal Success, failure_or_success.ev.class
    end

    def test_or_can_short_circuit
      success_or_erroneous = Or.new(Leaf.new(Success), Erroneous.new)
      assert_equal Success, success_or_erroneous.ev.class
    end

    def test_can_delay_outside
      outside = Delay.new(And.new(Leaf.new(Success), Leaf.new(Success)))
      assert_equal Success, outside.ev.step(outside).class
    end

    def test_can_delay_in_left_branch
      left = And.new(Delay.new(Leaf.new(Success)), Leaf.new(Success))
      assert_equal Success, left.ev.step(left).class
    end

    def test_can_delay_in_right_branch
      right = And.new(Leaf.new(Success), Delay.new(Leaf.new(Success)))
      assert_equal Success, right.ev.step(right).class
    end

  end
end
