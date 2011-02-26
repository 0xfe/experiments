#!/usr/bin/env python2.7

import unittest
from sqlalchemy import create_engine
from sqlalchemy.orm import mapper, sessionmaker

from model import APIFactory, ValidationError, metadata

class MockAPIFactory(APIFactory):
  def initialize_engine(self):
    self.engine = create_engine('sqlite:///:memory:', echo=False)

class TestUserAPI(unittest.TestCase):
  def setUp(self):
    api = MockAPIFactory()
    api.create_tables()
    self.user_api = api.get_user_api()

  def test_add_user(self):
    self.user_api.add_user(email="a@b.com", password="abc", name="Mo")
    self.assertRaises(ValidationError, self.user_api.add_user,
        email="a@b.com", password="abc")
    self.assertRaises(ValidationError, self.user_api.add_user, email="b@b.com")

  def test_update_user(self):
    self.user_api.add_user(email="a@b.com", password="abc", name="Mo")
    self.user_api.update_user(email="a@b.com", password="p4ssword")
    self.assertRaises(ValidationError, self.user_api.update_user,
        email="b@b.com", password="p4")

if __name__ == '__main__':
  unittest.main()
