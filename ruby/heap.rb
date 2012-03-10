#!/usr/bin/env ruby -w

module Vex

=begin
Implementation of ruby binary-heap / priority queue.
Author: Mohit Cheppudira <mohit@muthanna.com>

Usage:

  h = Vex::Heap.new

  h.empty?  # returns true

  h.insert(10)
  h.insert(20)
  h.insert(15)

  h.peek  # returns 20
  h.remove  # returns 20
  h.peek  # returns 15

  h.size  # returns 2
=end

class Heap
  attr_reader :data

  def initialize
    @data = []
  end

  def empty?
    @data.empty?
  end

  def size
    @data.size
  end

  def peek
    @data.first
  end

  def insert(item)
    @data << item
    pos = @data.length - 1
    while pos != 0
      parent = ((pos + 1) / 2).floor - 1

      if @data[parent] < @data[pos]
        @data[pos], @data[parent] = @data[parent], @data[pos]
        pos = parent
      else
        return
      end
    end
  end

  def remove
    return nil if empty?
    return @data.pop if size == 1

    # Move last value to root
    retval, @data[0] = @data[0], @data.pop

    pos = 0
    while true
      left = ((pos + 1) * 2) - 1
      right = left + 1
      next_child = left

      break if left >= size

      if right < size then
        next_child = right if @data[right] > @data[left]
      end

      if @data[next_child] > @data[pos] then
        @data[next_child], @data[pos] = @data[pos], @data[next_child]
        pos = next_child
      else
        break
      end
    end

    return retval
  end

  alias :pop :remove
  alias :push :insert
end
end

# Run tests if called directly

if __FILE__ != $0 then
  return
end

require 'pp'
require 'test/unit'

class TestHeap < Test::Unit::TestCase
  def test_deterministic
    h = Vex::Heap.new
    assert(h.empty?)
    (1..1000).each { |x| h.insert x; assert(h.peek == x) }
    999.downto(1).each { |x| h.remove; assert(h.peek == x) }
    assert_equal(1, h.size)
  end

  def test_random
    h = Vex::Heap.new
    assert_equal(0, h.size)
    1000.times { h.insert rand(1000); assert(h.peek == h.data.max) }
    1000.times {
      max = h.data.max
      assert(max == h.remove);
      assert(h.peek == h.data.max)
    }
    assert(h.empty?)
    assert(h.remove == nil)
  end
end


