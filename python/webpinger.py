#!/usr/bin/env python2.6

import string, cgi
import os
import re
import sys
import time
import threading
from BaseHTTPServer import BaseHTTPRequestHandler, HTTPServer
from SocketServer import ForkingMixIn
from threading import Thread

class CometHandler(BaseHTTPRequestHandler):
  def do_GET(self):
    if self.path.endswith(".html"):
      self.send_response(200)
      self.send_header('Content-type', "text/plain")
      self.end_headers()

      self.wfile.write("Thread: " + threading.currentThread().getName() + "\n")
      pinger = os.popen("ping muthanna.com")

      while 1:
        line = pinger.readline()
        if not line: break
        self.wfile.write(line)

    else:
      self.send_error(404, "Not found.")

  def do_POST(self):
    self.do_GET

class ThreadedHTTPServer(ForkingMixIn, HTTPServer):
  """Handles requests in a separate thread."""
  pass

def main():
  try:
    server = ThreadedHTTPServer(('', 3000), CometHandler)
    print "Server listening on port 3000"
    server.serve_forever()
  except KeyboardInterrupt:
    server.socket.close()

if __name__ == '__main__':
  main()
