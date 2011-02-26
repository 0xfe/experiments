#!/usr/bin/env python2.7
#
# Install sqlalchemy with easy_install or ports.

import json
import logging

from datetime import date, datetime

from sqlalchemy import create_engine, Table, Column, MetaData
from sqlalchemy import Boolean, Integer, String, Date, DateTime, Float, Enum
from sqlalchemy import Text, LargeBinary, Unicode
from sqlalchemy import ForeignKey
from sqlalchemy.orm import mapper, sessionmaker

from vexweb.db import ValidationError, DBObject

metadata = MetaData()

class User(DBObject):
  schema = [
        # Identity
        Column('id', Integer, primary_key=True),
        Column('login', String(100), nullable=False, unique=True),
        Column('email', String(100), nullable=False, unique=True),

        # Password
        Column('password', String(200), nullable=False),
        Column('password_encoding', Enum("SHA256", "ROT13"), 
               default="SHA256", nullable=False),

        # Name and pic
        Column('name', String(100), nullable=False), # Nick name
        Column('first_name', String(25), nullable=True),
        Column('middle_name', String(25), nullable=True),
        Column('last_name', String(25), nullable=True),
        Column('picture', LargeBinary, nullable=True),

        # Location
        Column('locale', String(20), default="en_US", nullable=False),
        Column('address1', Unicode(100), nullable=True),
        Column('address2', Unicode(100), nullable=True),
        Column('address3', Unicode(100), nullable=True),
        Column('address4', Unicode(100), nullable=True),
        Column('state', Unicode(30), nullable=True),
        Column('province', Unicode(30), nullable=True),
        Column('city', Unicode(30), nullable=True),
        Column('town', Unicode(30), nullable=True),
        Column('country', Unicode(30), nullable=True),
        Column('area_code', Unicode(20), nullable=True),
        Column('postal_code', Unicode(20), nullable=True),
        Column('pobox', Unicode(20), nullable=True),
        Column('lat', Float, nullable=True),
        Column('lng', Float, nullable=True),

        # Contact
        Column('phone', String(30), nullable=True),
        Column('mobile', String(30), nullable=True),
        Column('xmpp', String(100), nullable=True),

        # Personal
        Column('dob', Date, nullable=True),
        Column('sex', Enum("Male", "Female"), nullable=True),

        # Referrals
        Column('referred_by', String(100), nullable=True),
        Column('referrer_code', String(100), nullable=True),

        # Account state
        Column('enabled', Boolean, nullable=False, default=True),
        Column('valid', Boolean, nullable=False, default=True),
        Column('account_state', Enum("Active", "Pending", "Fraud"),
          nullable=True, default="Active"),

        # External ID
        Column('external_provider', String(100), nullable=True),
        Column('external_id', String(100), nullable=True),

        # Administrative
        Column('type', Enum("Standard", "Admin", "Retailer"),
               nullable=False, default="Standard"),
        Column('json_data', Text, nullable=True),
        Column('notes', Text, nullable=True),

        # Stats
        Column('date_created', DateTime, nullable=False,
               default=datetime.now),
        Column('date_modified', DateTime, nullable=False,
               default=datetime.now, onupdate=datetime.now),
        Column('date_last_login', DateTime, nullable=True),
  ]

  @classmethod
  def initialize_metadata(cls, metadata):
    return cls.initialize_table('users', metadata)

  def commit_session(self, session):
    self.login = self.email
    session.commit()

  def validate(self):
    if (self.name is None):
      raise ValidationError("Name required for user", "name")

    if (self.password is None):
      raise ValidationError("Password required for user", "password")

    if (self.password == ""):
      raise ValidationError("Password cannot be empty", "password")

    if (self.email is None or self.email == ""):
      raise ValidationError("E-mail cannot be empty", "email")


User.initialize_metadata(metadata)

class UserAPI(object):
  def __init__(self, session):
    self.session = session

  def __exists_user(self, email):
    return (self.session.query(User).filter_by(email=email).count() > 0)

  def __find_user(self, email):
    return self.session.query(User).filter_by(email=email).first()

  def find_user(self, email):
    user = self.__find_user(email)
    if (user == None):
      return None
    else:
      return user.to_struct()

  def add_user(self, **user_struct):
    user = User()
    user.update_from_struct(user_struct)
    user.validate() # raises exception

    email = user_struct['email']
    if (self.__exists_user(email)):
      raise ValidationError("User '%s' already exists." % email, "email")

    user.add_to_session(self.session)
    user.commit_session(self.session)
    return user.to_struct()

  def update_user(self, **user_struct):
    email = user_struct['email']
    user = self.__find_user(email)
    if (user is None):
      raise ValidationError("User '%s' does not exist." % email, "email")

    user.update_from_struct(user_struct)

    try:
      user.validate() # raises exception
    except Exception, e:
      self.session.rollback()
      raise

    user.commit_session(self.session)
    return user.to_struct()

class APIFactory(object):
  log_sql = False

  def __init__(self):
    self.initialize_engine()
    self.metadata = metadata
    self.Session = sessionmaker(bind=self.engine)
    self.session = self.Session()

  def initialize_engine(self, log_sql=False):
    self.engine = create_engine('sqlite:///:memory:', self.log_sql)

  def create_tables(self):
    self.metadata.create_all(self.engine)

  def get_user_api(self):
    return UserAPI(self.session)
