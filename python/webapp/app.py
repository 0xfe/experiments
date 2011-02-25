#!/usr/bin/env python2.7

# Built for tornado 1.2

import json
import logging
import os

import tornado.options
from tornado import auth, ioloop, web
from tornado.options import define, options
from vexweb.handlers import BaseHandler

from login import DBLoginHandler
from model import APIFactory

define("port", default=8888, help="HTTP listen port.")
define("cookie_secret", default="boo baa", help="Encrypted cookie key.")
tornado.options.parse_command_line()

logging.basicConfig(level=logging.INFO)

class Config:
  port = int(options.port)
  base_path = os.path.dirname(__file__)
  web_settings = {
      "static_path": os.path.join(base_path, "static"),
      "template_path": base_path + "/templates",
      "cookie_secret": options.cookie_secret,
      "login_url": "/login",
      "xsrf_cookies": True
    }

class MainHandler(BaseHandler):
  @web.authenticated
  def get(self):
    self.set_header("Content-Type", "text/html")
    action = self.get_argument("action", default="list")
    user = self.current_user
    self.render_template("frontpage.html", action=action, user=user)

if __name__ == "__main__":

  application = web.Application([
    (r"/", MainHandler),
    (r"/login", DBLoginHandler)
  ], **Config.web_settings)

  application.vexconfig = Config()

  api = APIFactory()
  api.create_tables()

  application.model_api = api
  application.listen(Config.port)

  logging.info("Starting HTTP Server on port %d" % Config.port)
  ioloop.IOLoop.instance().start()
