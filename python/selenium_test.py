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

import re
import time
import unittest

from selenium import webdriver, selenium

class BrowserTestCase(unittest.TestCase):
  @classmethod
  def get_chrome(cls):
    # don't use "*chrome"
    browser = selenium("localhost", 4444, "*googlechrome",
                           "http://localhost:4444")

    # this flag is needed to prevent chrome from crapping out for certain
    # pages
    browser.start('commandLineFlags=--disable-web-security')
    return browser

  @classmethod
  def get_firefox(cls):
    browser = selenium("localhost", 4444, "*firefox",
                           "http://localhost:4444")
    browser.start()
    return browser

  @classmethod
  def tearDownClass(cls):
    cls.browser.stop()


class TestGoogleSearchChrome(BrowserTestCase):
  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_chrome()

  def test_vanity_search(self):
    b = self.browser
    b.open("http://google.com")
    b.wait_for_page_to_load("10000")

    b.type("name=q", "0xfe")
    time.sleep(1)

    # browser.click("//input[@value='Search']")
    b.click("//input[contains(@value, 'Search')]")
    time.sleep(1)

    # text = browser.get_html_source()
    text = b.get_body_text()
    self.assertIsNotNone(re.search("muthanna", text))

class TestGoogleSearchFirefox(TestGoogleSearchChrome):
  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_firefox()

if __name__ == "__main__":
  unittest.main()
