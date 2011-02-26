#!/usr/bin/env python2.7
#
# Install sqlalchemy with easy_install or ports.

from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker

from models.metadata import metadata
from models import user

class APIFactory(object):
  log_sql = False

  def __init__(self):
    self.initialize_engine()
    self.metadata = metadata
    self.Session = sessionmaker(bind=self.engine)
    self.session = self.Session()

  def initialize_engine(self):
    self.engine = create_engine('sqlite:///:memory:', echo=self.log_sql)

  def create_tables(self):
    self.metadata.create_all(self.engine)

  def get_user_api(self):
    return user.API(self.session)
