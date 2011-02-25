#!/usr/bin/env python2.7

# Built for tornado 1.2

import json
import os

from tornado import auth, ioloop, web

class BaseHandler(web.RequestHandler):
  errors = []
  flash = []

  def get_config(self):
    return self.application.vexconfig

  def prepare(self):
    self.errors = []
    self.flash = []

  def add_error(self, error):
    self.errors.append(error)

  def get_model_api(self):
    return self.application.model_api

  def get_current_user(self):
    return self.get_secure_cookie("user")

  def set_current_user(self, user):
    self.set_secure_cookie("user", user)

  def render_ajax(self, success, data):
    self.write(json.dumps({
      "success": success,
      "data": data,
      "errors": errors,
      "flash": flash
      }))

  def render_template(self, path, **kwargs):
    return self.render(path,
                       errors=self.errors,
                       flash=self.flash,
                       **kwargs)


class GoogleLoginHandler(BaseHandler, auth.GoogleMixin):
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

    self.set_current_user(user["email"])
    self.redirect("/")
