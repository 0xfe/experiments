package com.muthanna.playground.scala

import java.io._;
import java.net._;
import java.util.Random;
import scala.collection.mutable.Map;
import scala.io.Source;
import scala.util.logging._;

object AsteriskTools {
  private val random = new Random()
  private val ACTIONID_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"

  // Generates a unique ActionID string for asterisk commands. The string
  // is composed of the characters in ACTIONID_CHARS
  def genActionId : String = (1 to 16).map(
    x => ACTIONID_CHARS(random.nextInt(ACTIONID_CHARS.length))).mkString

  def genCommand(command : Map[String, String]) : String =
    command.keys.map(k => k + ": " + command(k)).mkString("\n")
}

class AMIGateway(host: String, port: Int, username: String, password: String)
  extends Logged {
  private var socket: Socket = null;
  private var timeout_ms: Int = 500;
  private var writer : BufferedWriter = null;

  def connect {
    val address = new InetSocketAddress(host, port)
    socket = new Socket()
    socket.bind(null)
    log("Connecting to " + host + ":" + port.toString)
    socket.connect(address, timeout_ms)
    log("Connected")

    writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
  }

  def disconnect { socket.close }

  def write(s : String) {
    writer.write(s);
    writer.flush();
  }

  def getInputStream : java.io.InputStream = socket.getInputStream
}


object SocketClient {
  def main(args: Array[String]) {
    println(AsteriskTools.genActionId)
    println(AsteriskTools.genCommand(Map("Action" -> "login",
                                         "Username" -> "user")))

    val gateway = new AMIGateway("www.google.com", 80, "a", "b") with ConsoleLogger

    gateway.connect
    gateway.write("GET /\n");
    Source.fromInputStream(gateway.getInputStream).getLines.foreach(println)
    gateway.disconnect
  }
}
