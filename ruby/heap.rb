#!/usr/bin/env ruby -w
#
# Implementation of ruby binary-heap / priority queue.
# Author: Mohit Cheppudira <mohit@muthanna.com>
#
# Usage:
#
# h = Heap.new
#
# h.empty?  # returns true
#
# h.insert(10)
# h.insert(20)
# h.insert(15)
#
# h.peek  # returns 20
# h.remove  # returns 20
# h.peek  # returns 15
#
# h.size  # returns 2

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
      parent = get_parent pos

      if @data[parent] < @data[pos]
        @data[pos], @data[parent] = @data[parent], @data[pos]
        pos = parent
      else
        return
      end
    end
  end

  def remove
    return if empty?
    if size == 1 then
      @data = []
      return
    end

    # Move last value to root
    @data[0] = @data.pop

    pos = 0
    while true
      left = get_left_child pos
      right = left + 1
      next_child = left

      return if left >= size

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
  end

  def get_level(pos)
    return (Math.log(pos + 1) / Math.log(2)).floor
  end

  def get_parent(pos)
    return ((pos + 1) / 2).floor - 1
  end

  def get_left_child(pos)
    return ((pos + 1) * 2) - 1
  end

  private :get_level, :get_parent, :get_left_child
end

# Run tests if called directly

if __FILE__ != $0 then
  return
end

require 'pp'
require 'test/unit'

class TestHeap < Test::Unit::TestCase
  def test_random
    h = Heap::new
    assert_equal(0, h.size)
    1000.times { h.insert rand(1000); assert(h.peek == h.data.max) }
    1000.times { h.remove; assert(h.peek == h.data.max) }
    assert(h.empty?)
  end

  def test_deterministic
    h = Heap::new
    assert(h.empty?)
    (1..1000).each { |x| h.insert x; assert(h.peek == x) }
    999.downto(1).each { |x| h.remove; assert(h.peek == x) }
    assert_equal(1, h.size)
  end
end
