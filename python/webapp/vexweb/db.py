#!/usr/bin/env python2.7
#
# Install sqlalchemy with easy_install or ports.

import json

from sqlalchemy import Table, MetaData
from sqlalchemy.orm import mapper

class ValidationError(Exception):
  def __init__(self, text, column=None):
    Exception.__init__(self, text)
    self.column = column
    self.text = text

  def __str__(self):
    return "Error in column '%s': %s" % (self.column, self.text)

class DBObject(object):
  @classmethod
  def initialize_table(cls, table_name, metadata):
    cls.fields = [prop.name for prop in cls.schema]
    cls.metadata = metadata
    table = Table(table_name, cls.metadata, *cls.schema)
    mapper(cls, table)
    return cls.metadata

  def to_struct(self):
    struct = { '_type': self.__class__.__name__ }
    for f in self.fields:
      struct[f] = self.__getattribute__(f)

    return struct

  def update_from_struct(self, struct):
    for f in self.fields:
      if struct.has_key(f):
        self.__setattr__(f, struct[f])

    return self

  def commit_session(self, session):
    session.commit()

  def add_to_session(self, session):
    session.add(self)

  def __repr__(self):
    return json.dumps(self.to_struct())
