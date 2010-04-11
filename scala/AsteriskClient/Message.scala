package asterisk.ami

import scala.collection.mutable.Map;

abstract class Message {
  def map: Map[String, String]
}

class Command(val action: String, params: Map[String, String])
  extends Message {
  private var variables: Option[Map[String, String]] = None
  val actionid: String = Util.genActionId
  val map: Map[String, String] = params + ("Action" -> action,
                                           "ActionID" -> actionid)

  def mkString: String = Util.genCommand(map, variables)
  def setVariables(vars: Option[Map[String, String]]): Command = {
    variables = vars
    this
  }
  def setVariables(vars: Map[String, String]): Command = {
    setVariables(Some(vars))
  }
  def clearVariables: Command = setVariables(None)
  def getVariables: Option[Map[String, String]] = variables
}

package command {
  // Command: Login
  // Used to authenticate with an Asterisk server. The credentials here
  // should be setup in /etc/asterisk/manager.conf
  case class Login(val username: String, val secret: String)
    extends Command("Login", Map("Events" -> "off",
                                 "Username" -> username,
                                 "Secret" -> secret))

  // Command: Originate
  // This is used to originate a phone call to the channel specified in
  // parameter "channel".
  case class Originate(val channel: String,
                       val context: String,
                       val exten: String,
                       val callerid: String,
                       val timeout_ms: Int)
    extends Command("Originate", Map("Channel" -> channel,
                                     "Context" -> context,
                                     "Callerid" -> callerid,
                                     "Priority" -> "1",
                                     "Timeout" -> timeout_ms.toString))
}

abstract class Response extends Message {
  def response: String = map("Response")
  def message: String = map("Message")
  def actionid: String = map("ActionID")
}

package response {
  case class Success(val map: Map[String, String]) extends Response
  case class Error(val map: Map[String, String]) extends Response
  case class Other(val map: Map[String, String]) extends Response
}

object Response {
  def apply(map: Map[String, String]) : Response = map("Response") match {
    case "Success" => response.Success(map)
    case "Error" => response.Error(map)
    case _ => response.Other(map)
  }

  def apply(response: String) : Response =
    Response(Util.parseResponse(response))
}
