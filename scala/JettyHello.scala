package com.muthanna.playground.scala

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.server.handler.DefaultHandler

// This is written for Jetty 7

object JettyHello {
  def main(args: Array[String]) {
    var server = new Server(8080)
    var handler = new DefaultHandler()

    handler.setServer(server)

    server.start()
  }
}
