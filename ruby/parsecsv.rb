#!/usr/bin/env ruby

# Calculate mean of 4th column

sum = 0
num = 0

ARGF.each do |line|
  line.chomp!
  sum += line.split(/\s*,\s*/)[4].to_i
  num += 1
end

puts(sum / num)

