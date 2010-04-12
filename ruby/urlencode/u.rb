#!/usr/bin/ruby

require 'cgi'

# Encode all parameters
str = CGI.escape(ARGV.join(""))
puts str
