#!/usr/bin/ruby -w
#
# This is me trying to learn a little ruby syntax.
#
# Skew numbers are a number system just like binary numbers,
# except you can increment a skew number in O(1) rather than O(log(bits)).
# Skew numbers represent numbers as sum of numbers of the form 2^n-1.
#
# http://yuml.me/diagram/scruffy/class[Skew%7C%7CtoInteger();successor();successorhelper(rank);print()]%5E[Zero],[Skew]%5E[Nonzero%7Crank;rest%7C],[Nonzero]++-Skew].

module Skew

class Zero
  def toInteger
    0
  end

  def successor
    Nonzero.new(0, Zero.new)
  end

  def successorHelper(rank)
    Nonzero.new(0, Nonzero.new(rank, Zero.new))
  end
end

class Nonzero
  attr_reader :rank
  attr_reader :rest

  def initialize(rank, rest)
    @rank = rank
    @rest = rest
  end

  def toInteger
    # the rank-th number of the form 2^n-1, plus the rest
    ((1 << (rank + 1)) - 1) + rest.toInteger
  end

  def successor
    rest.successorHelper(rank)
  end

  def successorHelper(incoming)
    if incoming == rank
      Nonzero.new(rank + 1, rest)
    else
      Nonzero.new(0, Nonzero.new(incoming, Nonzero.new(rank, rest)))
    end
  end
end



end # of module



if __FILE__ == $0

  require "test/unit"

  class TestSkew < Test::Unit::TestCase
    def test_zero_is_zero
      assert_equal(0, Skew::Zero.new.toInteger())
    end

    def test_counting_to_three
      assert_equal(3, Skew::Zero.new.successor.successor.successor.toInteger())
    end

    def test_up_to_one_hundred
      for i in 1..100
        accumulator = Skew::Zero.new
        for j in 1..i
          accumulator = accumulator.successor
        end
        assert_equal(i, accumulator.toInteger())
      end
    end
  end

end
