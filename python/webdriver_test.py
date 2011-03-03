#!/usr/bin/env python2.7

# Install selenium client library:
#
#   $ easy_install2.7 selenium
#
# Docs:
#
#   http://code.google.com/p/selenium/wiki/PythonBindings
#   http://seleniumhq.org/docs/03_webdriver.html

import re
import time
import unittest

from selenium import webdriver

class BrowserTestCase(unittest.TestCase):
  @classmethod
  def get_chrome(cls):
    # don't use "*chrome"
    browser = webdriver.Chrome()
    return browser

  @classmethod
  def get_firefox(cls):
    browser = webdriver.Firefox()
    return browser

  @classmethod
  def get_remote(cls):
    browser = webdriver.Remote()
    return browser

  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_chrome()

  @classmethod
  def tearDownClass(cls):
    cls.browser.close()


class TestGoogleSearchChrome(BrowserTestCase):
  def test_vanity_search(self):
    b = self.browser
    b.get("http://google.com")

    search_box = b.find_element_by_name("q")
    search_box.send_keys("0xfe")

    time.sleep(1)

    # browser.click("//input[@value='Search']")
    button = b.find_elements_by_xpath("//input[contains(@value, 'Search')]")[0]
    button.click()

    time.sleep(1)

    # text = browser.get_html_source()
    text = b.get_page_source()
    self.assertIsNotNone(re.search("muthanna", text))

class TestGoogleSearchFirefox(TestGoogleSearchChrome):
  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_firefox()

if __name__ == "__main__":
  unittest.main()
