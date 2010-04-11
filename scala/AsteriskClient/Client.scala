package asterisk.ami

import java.io._;
import java.net._;
import java.util.Random;
import scala.collection.mutable.Map;
import scala.io.Source;
import scala.util.logging._;
import scala.util.matching.Regex;

case class ResponseError(val response: Response)  extends Exception
class Client(val conn: BaseConnection) {
  // Send AMI command to server, and return the response. This will wait
  // for the response with the same ActionID as the command.
  def sendCommand(command: Command) : Response = {
    conn.write(command.mkString)

    var response = Response(conn.readResponse)
    while (response.actionid != command.actionid) {
      response = Response(conn.readResponse)
    }

    response
  }

  // Same as sendCommand, except that a ResponseError exception is thrown
  // upon error.
  def tryCommand(cmd: Command) : Response = {
    val resp = sendCommand(cmd)
    resp match {
      case ami.response.Error(_) => throw ResponseError(resp)
      case _ => resp
    }
  }
}
