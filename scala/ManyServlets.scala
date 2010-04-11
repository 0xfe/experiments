package com.muthanna.playground.scala;

import java.io._;
import javax.servlet.http._;

import org.mortbay.jetty.Server;
import org.mortbay.jetty.handler.ContextHandlerCollection;
import org.mortbay.jetty.handler.StatisticsHandler;
import org.mortbay.jetty.servlet.Context;
import org.mortbay.jetty.servlet.ServletHolder;

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

object ManyServlets {
  def main(args: Array[String]) {
    val server = new Server(8080)
    val contexts = new ContextHandlerCollection()

    val root = new Context(contexts, "/", Context.SESSIONS)
    root.addServlet(new ServletHolder(new BasicServlet()), "/*")

    val other = new Context(contexts, "/other", Context.SESSIONS)
    other.addServlet(classOf[BasicServlet], "/*")

    val stats = new StatisticsHandler()
    contexts.addHandler(stats)

    server.setHandler(contexts)
    server.start()
  }
}
