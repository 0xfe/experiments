#!/usr/bin/env python

import readline

class Completer:
  def __init__(self, words):
    self.words = words
    self.prefix = None

  def complete(self, prefix, index):
    if prefix != self.prefix:
      self.matching_words = [ w for w in self.words if w.startswith(prefix) ]
      self.prefix = prefix
    try:
      return self.matching_words[index]
    except IndexError:
      return None


words = ["stuff", "others", "me", "monkeys", "othello"]
completer = Completer(words)

readline.parse_and_bind("tab: complete")
readline.set_completer(completer.complete)

line = ""

while line != "q":
  line = raw_input("> ")

  if line == "?":
    print "q: quit"
  else:
    print line
