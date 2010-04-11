package asterisk.ami

import java.io._;
import java.net._;
import scala.io.Source;
import scala.collection.mutable.Map;
import scala.util.logging._;

case class ConnectionException(val msg: String) extends Exception

abstract class BaseConnection {
  def connect
  def disconnect
  def write(s: String)
  def readResponse: String
  def isConnected: Boolean
}

class Connection(host: String, port: Int)
  extends BaseConnection with Logged {

  private var socket: Socket = null;
  private var timeout_ms: Int = 500;
  private var writer: BufferedWriter = null;
  private var reader: BufferedReader = null;

  var isConnected: Boolean = false;

  def connect {
    val address = new InetSocketAddress(host, port)

    socket = new Socket()
    socket.bind(null)
    log("Connecting to Asterisk Manager at " + host + ":" + port.toString)
    socket.connect(address, timeout_ms)
    log("Connected established to " + host + ":" + port)

    isConnected = true
    writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream))
    reader = new BufferedReader(new InputStreamReader(socket.getInputStream))

    // Read AMI header.
    log("Received Asterisk header: " + reader.readLine)
  }

  def disconnect {
    if (isConnected) {
      log("Disconnecting from Asterisk Manager at " + host + ":" + port)
      socket.close
    }
    isConnected = false
  }

  def write(s: String) {
    if (!isConnected) throw ConnectionException("Not connected")
    log("Sending AMI message to " + host + ":" + port + ": ")
    log(s)
    writer.write(s);
    writer.flush();
  }

  def readResponse: String = {
    if (!isConnected) throw ConnectionException("Not connected")

    var line: String = reader.readLine
    var block: String = ""

    while (!line.isEmpty) {
      log(host + ":" + port + " says: " + line)
      block += line + "\n"
      line = reader.readLine
    }

    block
  }
}
