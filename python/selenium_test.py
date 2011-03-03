#!/usr/bin/env python2.7

# Install selenium client library:
#
#   $ easy_install2.7 selenium
#
# Requires:
#
#  $ curl -O http://selenium.googlecode.com/files/selenium-server-standalone-2.0b2.jar
#
# Make sure server is running:
#
#  $ java -jar selenium-server-standalone.jar
#
# Docs:
#
#   http://code.google.com/p/selenium/wiki/PythonBindings
#   http://release.seleniumhq.org/selenium-core/1.0/reference.html

from selenium import webdriver, selenium
import re
import time

# don't use "*chrome"
s = selenium("localhost", 4444, "*googlechrome", "http://localhost:4444")

# this flag is needed to prevent chrome from crapping out for certain
# pages
s.start('commandLineFlags=--disable-web-security')
s.open("http://google.com")
s.wait_for_page_to_load("10000")

s.type("name=q", "0xfe")
time.sleep(1)

# s.click("//input[@value='Search']")
s.click("//input[contains(@value, 'Search')]")
time.sleep(1)

# text = s.get_html_source()
text = s.get_body_text()
if re.search("muthanna", text) is not None:
  print "PASS"
else:
  print "FAIL"

s.stop()
