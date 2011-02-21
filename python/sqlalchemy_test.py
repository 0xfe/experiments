#!/usr/bin/env python2.7
#
# Install sqlalchemy with easy_install or ports.

import json
import logging
from sqlalchemy import create_engine, Table, Column, Integer, String, MetaData
from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapper, sessionmaker

class User(object):
  def to_struct(self):
    return {
        'type': 'user',
        'id': self.id,
        'name': self.name,
        }

  def __init__(self, name):
    self.name = name

  def __repr__(self):
    return json.dumps(self.to_struct())

# Create an API out of it, to abstract away the storage layer.

class UserAPIException(Exception):
  pass

class UserAPI(object):
  def __init__(self):
    engine = create_engine('sqlite:///:memory:', echo=True)

    metadata = MetaData()
    users_table = Table('users', metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String))

    metadata.create_all(engine)
    mapper(User, users_table)

    Session = sessionmaker(bind=engine)
    self.session = Session()

  def FindUser(self, name):
    user = self.session.query(User).filter_by(name=name).first()
    if (user == None):
      return None
    else:
      return user.to_struct()

  def AddUser(self, name):
    if (self.FindUser(name) != None):
      raise UserAPIException("User '%s' already exists." % name)

    user = User(name)
    self.session.add(user)
    self.session.commit()


try:
  user_api = UserAPI()
  user_api.AddUser('mo')

  mo = user_api.FindUser('mo')
  print mo

  user_api.AddUser('mo')
except Exception, e:
  logging.error(e)
