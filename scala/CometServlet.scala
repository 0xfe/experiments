package com.muthanna.playground.scala;

import java.io._;
import java.util.Date;
import javax.servlet.http._;

import org.mortbay.jetty.Connector;
import org.mortbay.jetty.Server;
import org.mortbay.jetty.bio.SocketConnector;
import org.mortbay.jetty.servlet.ServletHandler;
import org.mortbay.thread._;

import org.mortbay.util.ajax._;

// For Jetty 6.
//
// To compile:
// $ fsc -cp '/opt/local/temp/jetty-6.1.19/lib/*' CometServlet.scala
//
// To run:
// $ scala -cp '/opt/local/temp/jetty-6.1.19/lib/*:.' \
//   com.muthanna.playground.scala.CometServlet

class BasicServlet extends HttpServlet {
  override def doGet(req: HttpServletRequest,
                     res: HttpServletResponse) : Unit = {
    res.setContentType("text/plain")
    res.setStatus(HttpServletResponse.SC_OK)
    var pw = res.getWriter

    pw.flush

    for (i <- 1 until 50) {
      pw.println(i.toString)
      pw.flush
      Thread.sleep(1000)
    }
  }

  override def doPost(req: HttpServletRequest,
                      res: HttpServletResponse) : Unit = doGet(req, res)
}

// This servlet does the same thing as the BasicServlet, except that
// it uses continuations to block the request, as opposed to putting the
// thread to sleep.
class ContinuationServlet extends HttpServlet {
  override def doGet(req: HttpServletRequest,
                     res: HttpServletResponse) : Unit = {
    res.setContentType("text/plain")
    res.setStatus(HttpServletResponse.SC_OK)
    val pw = res.getWriter

    pw.println("Request: " + new Date())
    pw.flush

    val cc = ContinuationSupport.getContinuation(req, null)

    for (i <- 1 until 50) {
      pw.println(i.toString)
      pw.flush
      cc.suspend(1000)
    }
  }

  override def doPost(req: HttpServletRequest,
                      res: HttpServletResponse) : Unit = doGet(req, res)
}

object CometServlet {
  def main(args: Array[String]) {
    val server = new Server(8080)
    val servlet_handler = new ServletHandler()

    servlet_handler.addServletWithMapping(classOf[BasicServlet], "/basic")
    servlet_handler.addServletWithMapping(
      classOf[ContinuationServlet], "/cont")
    server.setHandler(servlet_handler)
    server.setThreadPool(new QueuedThreadPool(100))
    server.start()
  }
}
