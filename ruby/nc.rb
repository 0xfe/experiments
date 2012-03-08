#!/usr/bin/env ruby

# ./nc.rb localhost 22

require 'socket'

host, port = ARGV

s = TCPSocket.open(host, port)

total_bytes = 0

while !s.eof?
  data = s.readline
  total_bytes += data.length
  puts data
end

s.close
puts "\n\nRead #{data.length} bytes."
