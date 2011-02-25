#!/usr/bin/env python2.7

# Built for tornado 1.2

import os
import logging

from tornado import auth, ioloop, web
from model import APIFactory

class Config:
  port = 8888
  base_path = os.path.dirname(__file__)
  web_settings = {
      "static_path": os.path.join(base_path, "static"),
      "cookie_secret": "boo baa baa",
      "login_url": "/login",
      "xsrf_cookies": True
    }

class BaseHandler(web.RequestHandler):
  def get_model_api(self):
    return self.application.model_api

  def get_current_user(self):
    return self.get_secure_cookie("user")

  def render_template(self, path, **kwargs):
    return self.render(
        os.path.join(Config.base_path, "templates", path), **kwargs)

class SimpleLoginHandler(BaseHandler):
  def get(self):
    self.render_template("login.html")

  def post(self):
    self.set_secure_cookie("user", self.get_argument("email"))
    self.redirect("/")

class DBLoginHandler(BaseHandler):
  def get(self):
    self.render_template("login.html")

  def post(self):
    api = self.get_model_api().get_user_api()
    email = self.get_argument("email")

    user = api.find_user(email)

    if (user):
      self.set_secure_cookie("user", self.get_argument("email"))
      self.redirect("/")
    else:
      self.redirect("/login")

class GoogleHandler(web.RequestHandler, auth.GoogleMixin):
  @web.asynchronous
  def get(self):
    if self.get_argument("openid.mode", None):
      self.get_authenticated_user(self.async_callback(self._on_auth))
      return

    self.authenticate_redirect()

  def _on_auth(self, user):
    if not user:
      logging.error("Login failed")
      self.authenticate_redirect()
      return

    self.set_secure_cookie("user", user["email"])
    self.redirect("/")

class RegisterHandler(BaseHandler):
  @web.authenticated
  def post(self):
    self.set_header("Content-Type", "text/html")

    api = self.get_model_api().get_user_api()
    email = self.get_argument("email", default=None)

    api.add_user({
      "name": self.get_argument("name", default=None),
      "email": self.get_argument("email", default=None),
      "password": self.get_argument("password", default=None),
      "notes": self.get_argument("notes", default=None)
    })

    self.set_secure_cookie("user", email)
    self.redirect("/")

class MainHandler(BaseHandler):
  @web.authenticated
  def get(self):
    self.set_header("Content-Type", "text/html")
    action = self.get_argument("action", default="list")
    user = self.current_user
    self.render_template("frontpage.html", action=action, user=user)

application = web.Application([
  (r"/", MainHandler),
  (r"/login", DBLoginHandler),
  (r"/register", RegisterHandler)
], **Config.web_settings)

if __name__ == "__main__":
  api = APIFactory()
  api.create_tables()

  application.model_api = api
  application.listen(Config.port)
  ioloop.IOLoop.instance().start()
