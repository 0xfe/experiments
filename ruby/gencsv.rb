#!/usr/bin/env ruby
# Generate a CSV: 100 lines of 10 random numbers per-line
1.upto(100) { puts ((1..10).map {|x| rand(200)}).join(", ") }
