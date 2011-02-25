#!/usr/bin/env python2.7
#
# Install sqlalchemy with easy_install or ports.

import json

class ValidationError(Exception):
  def __init__(self, text, column=None):
    Exception.__init__(self, text)
    self.column = column
    self.text = text

  def __str__(self):
    return "Error in column '%s': %s" % (self.column, self.text)

class DBObject(object):
  def to_struct(self):
    struct = { 'type': self.__class__.__name__ }
    for f in self.fields:
      struct[f] = self.__getattribute__(f)

    return struct

  def update_from_struct(self, struct):
    for f in self.fields:
      if struct.has_key(f):
        self.__setattr__(f, struct[f])

    return self

  def __repr__(self):
    return json.dumps(self.to_struct())
