#!/usr/bin/env python2.7

# Built for tornado 1.2

import tornado.ioloop
import tornado.web

class MainHandler(tornado.web.RequestHandler):
  def get(self):
    self.write("Hello world!")

application = tornado.web.Application([
  (r"/", MainHandler),
])

if __name__ == "__main__":
  application.listen(8888)
  tornado.ioloop.IOLoop.instance().start()
