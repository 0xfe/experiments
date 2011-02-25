#!/usr/bin/env python2.7
#
# Install sqlalchemy with easy_install or ports.

import json
import logging
from sqlalchemy import create_engine, Table, Column, Integer, String, MetaData
from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapper, sessionmaker

LOG_SQL = False

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

class User(DBObject):
  fields = ['id', 'email', 'name', 'password', 'notes']

  @classmethod
  def initialize_metadata(cls):
    cls.metadata = MetaData()
    users_table = Table('users', cls.metadata,
        Column('id', Integer, primary_key=True),
        Column('email', String, nullable=False, unique=True),
        Column('name', String, nullable=False),
        Column('password', String, nullable=False),
        Column('notes', String),
    )
    mapper(cls, users_table)
    # cls.fields = [prop.key for prop in mapper(cls).iterate_properties
        # if isinstance(prop, sqlalchemy.orm.ColumnProperty)]
    return cls.metadata

  def validate(self):
    if (self.name is None):
      raise ValidationError("Name required for user", "name")

    if (self.password is None):
      raise ValidationError("Password required for user", "password")

    if (self.password == ""):
      raise ValidationError("Password cannot be empty", "password")

    if (self.email is None or self.email == ""):
      raise ValidationError("E-mail cannot be empty", "email")


# Create an API out of it, to abstract away the storage layer.
class UserAPIException(Exception):
  pass

class UserAPI(object):
  def __init__(self, session):
    self.session = session

  def __find_user(self, email):
    return self.session.query(User).filter_by(email=email).first()

  def find_user(self, email):
    user = self.__find_user(email)
    if (user == None):
      return None
    else:
      return user.to_struct()

  def add_user(self, user_struct):
    user = User()
    user.update_from_struct(user_struct)
    user.validate() # raises exception

    email = user_struct['email']
    if (self.__find_user(email) != None):
      raise UserAPIException("User '%s' already exists." % email)

    self.session.add(user)
    self.session.commit()
    return user.to_struct()

  def update_user(self, user_struct):
    user = self.__find_user(user_struct['email'])
    if (user is None):
      raise UserAPIException("User '%s' does not exist." % email)

    user.update_from_struct(user_struct)

    try:
      user.validate() # raises exception
    except Exception, e:
      self.session.rollback()
      raise

    self.session.commit()
    return user.to_struct()

class APIFactory(object):
  def __init__(self):
    self.engine = create_engine('sqlite:///:memory:', echo=LOG_SQL)
    self.metadata = User.initialize_metadata()
    self.Session = sessionmaker(bind=self.engine)
    self.session = self.Session()

  def create_tables(self):
    self.metadata.create_all(self.engine)

  def get_user_api(self):
    return UserAPI(self.session)
