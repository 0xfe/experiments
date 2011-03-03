#!/usr/bin/env ruby

# Installation on mac:
#
# $ sudo gem update --system
# $ sudo gem install watir-webdriver

require "rubygems"
require "watir-webdriver"

def assert(val, msg)
  if not val
    puts "ERROR: " + msg
  end
end

class GoogleSearch
  attr_reader :browser

  def initialize(browser)
    @browser = browser
  end

  def test_vanity_search
    @browser.goto("http://google.com")
    @browser.text_field(:name => "q").set "0xfe"

    # Match on /Search/ because of Google Instant - its buttons are
    # "Google Search" or "Search"
    @browser.button(:value => /Search/).click()

    assert(@browser.text.include?('muthanna'), "No muthanna found")
  end

  def test_reddit
    @browser.goto("http://reddit.com/r/programming")
    assert(@browser.text.include?('PROGRAMMING'), "Not Programming")
  end
end

browser = Watir::Browser.new(:chrome)
search = GoogleSearch.new(browser)

search.test_vanity_search
search.test_reddit

browser.close
