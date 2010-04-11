#!/usr/bin/ruby

require 'cgi'

str = CGI.escape(ARGV.join(""))

puts str
