#!/usr/bin/ruby -w
#
# This is me trying to learn a little ruby syntax.
#
# Skew numbers are a number system just like binary numbers,
# except you can increment a skew number in O(1) rather than O(log(bits)).
# Skew numbers represent numbers as sum of numbers of the form 2^n-1.
#
# http://yuml.me/diagram/scruffy/class/[Skew%7C%7Cto_i();successor();successorhelper(rank);print()]%5E[Zero],[Skew]%5E[Nonzero%7Crank;rest%7C],[Nonzero]++-[Skew].

module Skew
  class Zero
    def to_i
      0
    end

    def successor
      Nonzero.new(0, Zero.new)
    end

    def successor_helper(rank)
      Nonzero.new(0, Nonzero.new(rank, Zero.new))
    end
  end

  class Nonzero
    attr_reader :rank, :rest

    def initialize(rank, rest)
      @rank = rank
      @rest = rest
    end

    def to_i
      # the rank-th number of the form 2^n-1, plus the rest
      ((1 << (rank + 1)) - 1) + rest.to_i
    end

    def successor
      rest.successor_helper(rank)
    end

    def successor_helper(incoming)
      if incoming == rank
        Nonzero.new(rank + 1, rest)
      else
        Nonzero.new(0, Nonzero.new(incoming, Nonzero.new(rank, rest)))
      end
    end
  end
end



if __FILE__ == $0
  require "test/unit"

  class TestSkew < Test::Unit::TestCase
    def test_zero_is_zero
      assert_equal 0, Skew::Zero.new.to_i
    end

    def test_counting_to_three
      assert_equal 3, Skew::Zero.new.successor.successor.successor.to_i
    end

    def test_up_to_one_hundred
      100.times do |i|
        accumulator = Skew::Zero.new
        (1..i).each do |j|
          accumulator = accumulator.successor
        end
        assert_equal i, accumulator.to_i
      end
    end
  end
end
