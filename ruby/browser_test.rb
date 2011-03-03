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

    button = @browser.button(:value => /Search/)
    if not button
      puts "Error: Couldn't find search button"
    end

    begin
      button.click()
    rescue Selenium::WebDriver::Error::ElementNotDisplayedError
      puts "Couldn't find click button."
    end

    assert(@browser.text.include?('muthanna'), "No muthanna found")
  end

  def test_reddit
    @browser.goto("http://reddit.com/r/programming")
    assert(@browser.text.include?('Programming'), "Not Programming")
  end
end

[:chrome, :firefox].each { |browser|
  b = Watir::Browser.new(browser)
  search = GoogleSearch.new(b)

  search.test_vanity_search
  search.test_reddit

  b.close
}
