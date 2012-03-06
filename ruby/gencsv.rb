#!/usr/bin/env ruby
# Generate a CSV: 100 lines of 10 random numbers per-line
100.times { puts ((1..10).map {rand(200)}).join(", ") }
