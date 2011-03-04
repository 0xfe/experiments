#!/usr/bin/env python2.7

# Install selenium client library:
#
#   $ easy_install2.7 selenium
#
# You need the standalone selenium server to be running for HTMLUnit tests.
#
# Docs:
#
#   http://code.google.com/p/selenium/wiki/PythonBindings
#   http://seleniumhq.org/docs/03_webdriver.html
#
# API Reference:
#
#   http://code.google.com/p/selenium/source/browse/trunk/py
#        .../selenium/webdriver/remote/webdriver.py
#   http://code.google.com/p/selenium/source/browse/trunk/py
#        .../selenium/webdriver/remote/webelement.py

import re
import time
import unittest
import logging

from datetime import datetime
from selenium import webdriver
from selenium.webdriver.common.exceptions import NoSuchElementException

class TimeoutException(Exception):
  pass

class BrowserTestCase(unittest.TestCase):
  wait_time = 5.0

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
    browser = webdriver.Remote(browser_name="htmlunit")
    return browser

  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_remote()

  @classmethod
  def tearDownClass(cls):
    cls.browser.quit()

  def wait_for(self, f, *args):
    done = False
    element = None
    start = datetime.now()

    while not done:
      try:
        element = f(*args)
      except Exception, e:
        logging.info(e)

      if element:
        done = True
      else:
        end = datetime.now()
        if (end - start).total_seconds() > self.wait_time:
          raise TimeoutException()

        time.sleep(0.25)

    return element

  def page_contains(self, text):
    source = self.browser.get_page_source()
    return re.search(text, source)

class TestGoogleSearch(BrowserTestCase):
  def test_vanity_search(self):
    b = self.browser
    b.get("http://google.com")

    search_box = b.find_element_by_name("q")
    search_box.send_keys("0xfe")

    button = self.wait_for(b.find_element_by_xpath,
                           "//input[contains(@value, 'Search')]")
    self.wait_for(button.is_displayed)
    button.click()

    me = self.wait_for(self.page_contains, "muthanna")
    self.assertIsNotNone(me)

class TestGoogleSearchFirefox(TestGoogleSearch):
  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_firefox()

class TestGoogleSearchChrome(TestGoogleSearch):
  @classmethod
  def setUpClass(cls):
    cls.browser = cls.get_chrome()

if __name__ == "__main__":
  unittest.main()
