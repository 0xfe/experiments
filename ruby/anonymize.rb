#!/usr/bin/env ruby1.9
#
# anonymize.rb - Mohit Muthanna Cheppudira 2012
# Quick and dirty message anonymizer.
#
# Replace private information in a given file with alternatives as specified
# in the "replacments" hash below, taking care to maintain string case.
#
# Not very robust, but does the job.
#
# Usage:
#   $ anonymize file.txt >anonymized_file.txt

require 'pp'

replacements = {
  "Mohit Muthanna Cheppudira" => "Anonymous Overweight Coward",
  "Amrita Haar Cheppudira" => "Incredulous Bitter Hog",
  "Arkin Ponnappa Cheppudira" => "Insatiable Little Monstrosity",
  "mmuthanna" => "joesmith",
  "google.com" => "avaliddomain.com",
  "Brooklyn" => "Katmandu",
  "Queens" => "Timbaktu",
  "Manhattan" => "Atlanta",
  "prospect" => "Tenth",
  "park" => "Sixth",
  "Ninth" => "Fourth",
  "Eighth" => "Seventh",
  "76" => "1887",
  "111" => "444",
  "NY" => "OK",
  "NJ" => "WA",
  "9C" => "8A",
  "10B" => "14L",
  "545" => "325",
  "565" => "433",
  "3450" => "9987",
  "1668" => "8774",
  "646" => "433",
  "11238" => "44321",
  "10011" => "45992",
  "10113" => "13221"
}

cache = {}

replacements.each do |name, replacement|
  name.split.zip(replacement.split).each do |part|
    cache[part.first.downcase] = part.last.downcase
    cache[part.first.upcase] = part.last.upcase
    cache[part.first.capitalize] = part.last.capitalize
  end
end

if ARGV.empty?
  puts "Usage: #{$0} filename ..."
  exit -1
end

ARGF.each_line do |line|
  cache.each do |name, replacement|
    line.gsub!(name, replacement)
  end

  print line
end
