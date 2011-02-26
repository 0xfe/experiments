#!/usr/bin/env python2.7

# Built for tornado 1.2

import json
import os

from tornado import auth, ioloop, web

class BaseHandlerException(Exception):
  pass

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

  def add_flash(self, flash):
    self.flash.append(flash)

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

  def process_fail(self, message):
    raise BaseHandlerException(
        "Error processing 'action' parameter: %s" % message)

  def process_request(self, request_type):
    action = self.get_argument("action", None)
    if not action:
      self.process_fail("Missing parameter: 'action'")
      return

    try:
      action_func = self.__getattribute__("%s_%s" % (request_type, action))
    except Exception, e:
      self.process_fail(e)
      return

    action_func()

  def get(self):
    return self.process_request("GET")

  def post(self):
    return self.process_request("POST")

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
