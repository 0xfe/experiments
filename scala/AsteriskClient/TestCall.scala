package asterisk.tools;

import scala.util.logging._;
import asterisk.ami;
import asterisk.ami.command;
import scala.collection.mutable.Map;

object TestCall {
  def main(args: Array[String]) {
    val conn = new ami.Connection("localhost", 5038) with ConsoleLogger
    val client = new ami.Client(conn)

    try {
      // Connect to AMI server.
      conn.connect()

      // Login to gateway.
      client.tryCommand(command.Login("phone", "ph0ne"))

      // Originate phone call.
      client.tryCommand(
        command.Originate(
          "SIP/12125656156@google_ame",
          "googletest",
          "s",
          "12125656156",
          30000).setVariables(Map("A" -> "B",
                                  "C" -> "D")))

    } catch {
      case e: ami.ConnectionException => println("Not connected.")
      case e: ami.ResponseError => println(
        "Command error: " + e.response.message)
    } finally {
      conn.disconnect
    }
  }
}
