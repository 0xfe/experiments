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
  pass

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
  fields = ['id', 'name', 'password', 'email', 'notes']

  @classmethod
  def initialize_metadata(cls):
    cls.metadata = MetaData()
    users_table = Table('users', cls.metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String),
        Column('password', String),
        Column('email', String),
        Column('notes', String),
    )
    mapper(cls, users_table)
    # cls.fields = [prop.key for prop in mapper(cls).iterate_properties
        # if isinstance(prop, sqlalchemy.orm.ColumnProperty)]
    return cls.metadata

  def validate(self):
    if (self.password is None):
      raise ValidationError("Password required for user")

    if (self.password == ""):
      raise ValidationError("Password cannot be empty")

    if (self.email == ""):
      raise ValidationError("E-mail cannot be empty")


# Create an API out of it, to abstract away the storage layer.

class UserAPIException(Exception):
  pass

class UserAPI(object):
  def __init__(self, session):
    self.session = session

  def __find_user(self, name):
    return self.session.query(User).filter_by(name=name).first()

  def find_user(self, name):
    user = self.__find_user(name)
    if (user == None):
      return None
    else:
      return user.to_struct()

  def add_user(self, name, password, email):
    if (self.__find_user(name) != None):
      raise UserAPIException("User '%s' already exists." % name)

    user = User()
    user.name = name
    user.password = password
    user.email = email
    user.validate() # raises exception

    self.session.add(user)
    self.session.commit()
    return user.to_struct()

  def update_user(self, name, user_struct):
    user = self.__find_user(name)
    if (user is None):
      raise UserAPIException("User '%s' does not exist." % name)

    user.update_from_struct(user_struct)

    try:
      user.validate() # raises exception
    except Exception, e:
      self.session.rollback()
      raise

    self.session.commit()
    return user.to_struct()

class API(object):
  def __init__(self):
    self.engine = create_engine('sqlite:///:memory:', echo=LOG_SQL)
    self.user_metadata = User.initialize_metadata()
    self.Session = sessionmaker(bind=self.engine)
    self.session = self.Session()

  def create_tables(self):
    self.user_metadata.create_all(self.engine)

  def get_user_api(self):
    return UserAPI(self.session)

api = API()
api.create_tables()
user_api = api.get_user_api()

print "Adding user 'mo'"
user_api.add_user('mo', 'p4d', 'mo@me.com')
mo = user_api.find_user('mo')
print mo

print "Expecting ValidationError"
try:
  user_api.add_user('go', '', 'mo@me.com')
except ValidationError, e:
  logging.error(e)

print "Expecting UserAPIException"
try:
  user_api.add_user('mo', 'go', 'mo@me.com')
except UserAPIException, e:
  logging.error(e)

print "Updating password for 'mo'"
user_api.update_user('mo', {'password': 'boo'})
mo = user_api.find_user('mo')
print mo

