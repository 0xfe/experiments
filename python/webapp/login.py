#!/usr/bin/env python2.7

# Built for tornado 1.2

from vexweb.handlers import BaseHandler, GET, POST

class DBLoginHandler(BaseHandler):
  def process_fail(self, message=None):
    self.render_template("login.html", login_url=self.get_login_url())

  def _login(self):
    api = self.get_model_api().get_user_api()
    email = self.get_argument("email", None)

    if not email:
      self.add_error("Missing argument: 'email'")
      return None

    user = api.find_user(email)
    if user:
      self.set_current_user(self.get_argument("email"))

    return user

  def _register(self):
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
      return email
    except Exception, e:
      self.add_error(str(e))
      return None

  @POST
  def login(self):
    user = self._login()

    if (user):
      self.redirect("/")
    else:
      self.add_error("Login failed. User does not exist.")
      self.process_fail()

  @GET
  @POST
  def ajax_login(self):
    user = self._login()

    if (user):
      self.render_ajax(True, user)
    else:
      self.add_error("Login failed. User does not exist.")
      self.render_ajax(False)

  @GET
  @POST
  def ajax_register(self):
    user = self._register()

    if user:
      self.render_ajax(True, user)
    else:
      self.render_ajax(False)
