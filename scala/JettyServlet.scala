package com.muthanna.playground.scala;

import java.io._;
import javax.servlet.http._;

import org.mortbay.jetty.Connector;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.bio.SocketConnector;
import org.mortbay.jetty.servlet.ServletHandler;

// For Jetty 6.
//
// To compile:
// $ fsc -cp '/opt/local/temp/jetty-6.1.19/lib/*' JettyServlet.scala
//
// To run:
// $ scala -cp '/opt/local/temp/jetty-6.1.19/lib/*:.' \
//   com.muthanna.playground.scala.JettyServlet

class BasicServlet extends HttpServlet {
  override def doGet(req: HttpServletRequest,
                     res: HttpServletResponse) : Unit = {
    res.setContentType("text/html")
    res.setStatus(HttpServletResponse.SC_OK)
    var pw = res.getWriter
    pw.println("<html>Hello World!</html>")
  }

  override def doPost(req: HttpServletRequest,
                      res: HttpServletResponse) : Unit = doGet(req, res)
}

object JettyServlet {
  def main(args: Array[String]) {
    val server = new Server(8080)
    val servlet_handler = new ServletHandler()

    servlet_handler.addServletWithMapping(classOf[BasicServlet], "/*")
    server.setHandler(servlet_handler)
    server.start()
  }
}
