#!/usr/bin/env ruby

# ./wget.rb www.google.com /

require 'socket'

host, path = ARGV
port = 80

s = TCPSocket.open(host, port)

s.puts "GET #{path}"

total_bytes = 0

while !s.eof?
  data = s.readline
  total_bytes += data.length
  puts data
end

s.close
puts "\n\nRead #{data.length} bytes."
