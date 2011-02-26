#!/usr/bin/env python2.7

# Built for tornado 1.2

from vexweb.handlers import BaseHandler

class DBLoginHandler(BaseHandler):
  def get(self):
    self.render_template("login.html", login_url=self.get_login_url())

  def register(self):
    api = self.get_model_api().get_user_api()
    email = self.get_argument("email", default=None)

    try:
      api.add_user(
        name=self.get_argument("name", default=None),
        email=self.get_argument("email", default=None),
        password=self.get_argument("password", default=None),
        notes=self.get_argument("notes", default=None)
      )

      self.set_current_user(email)
      self.redirect("/")
    except Exception, e:
      self.add_error(e)
      self.get()

  def login(self):
    api = self.get_model_api().get_user_api()
    email = self.get_argument("email")

    user = api.find_user(email)

    if (user):
      self.set_current_user(self.get_argument("email"))
      self.redirect("/")
    else:
      self.add_error("Login failed. User does not exist.")
      self.get()

  def post(self):
    action = self.get_argument("action", "login")

    if (action == "register"):
      self.register()
    else:
      self.login()
