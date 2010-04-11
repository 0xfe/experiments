/*
 * AMIConnection.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package asteriskclient

import java.io._;
import java.net._;
import scala.io.Source;
import scala.collection.mutable.Map;
import scala.util.logging._;

case class AMIConnectionException(val msg: String) extends Exception

abstract class BaseAMIConnection {
  def connect
  def disconnect
  def write(s: String)
  def readBlock: String
  def isConnected: Boolean
}

class AMIConnection(host: String, port: Int) 
  extends BaseAMIConnection with Logged {
  
  type AsteriskMessage = Map[String, String]
  private var socket: Socket = null;
  private var timeout_ms: Int = 500;
  private var writer: BufferedWriter = null;
  private var reader: BufferedReader = null;
  private var connected: Boolean = false;

  def connect {
    val address = new InetSocketAddress(host, port)

    socket = new Socket()
    socket.bind(null)
    log("Connecting to Asterisk Manager at " + host + ":" + port.toString)
    socket.connect(address, timeout_ms)
    log("Connected established to " + host + ":" + port)

    connected = true
    writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
    reader = new BufferedReader(new InputStreamReader(socket.getInputStream))
  }

  def disconnect {
    if (connected) {
      log("Disconnecting from Asterisk Manager at " + host + ":" + port)
      socket.close
    }
    connected = false
  }

  def write(s: String) {
    if (!connected) throw AMIConnectionException("Not connected")
    log("Sending AMI message to " + 
        host + ":" + port + ": " + s)
    writer.write(s);
    writer.flush();
  }

  def readBlock: String = {
    if (!connected) throw AMIConnectionException("Not connected")
    
    var line: String = reader.readLine
    var block: String = ""

    while (!line.isEmpty) {
      log("Received line from " + host + ":" + port + ": " + line)
      block += line + "\n"
      line = reader.readLine
    }

    block
  }

  def isConnected: Boolean = connected
}