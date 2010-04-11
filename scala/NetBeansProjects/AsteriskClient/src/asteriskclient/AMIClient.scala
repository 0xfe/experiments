/*
 * AMIClient.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package asteriskclient

import java.io._;
import java.net._;
import java.util.Random;
import scala.collection.mutable.Map;
import scala.io.Source;
import scala.util.logging._;
import scala.util.matching.Regex;

case class AMIClientException(val msg: String) extends Exception
class AMIClient(conn: BaseAMIConnection) {
  type AsteriskMessage = Map[String, String]

  def command(cmd: AsteriskMessage,
              vars: Map[String, String]): AsteriskMessage = {
    cmd += ("ActionID" -> Util.genActionId)
    if (vars != null) cmd += ("Variable" -> Util.genVariables(vars))

    conn.write(Util.genCommand(cmd))
    return Util.parseResponse(conn.readBlock)
  }

  def login(username: String, password: String) {
    val response = command(Map("Action" -> "login",
                               "Events" -> "off",
                               "Username" -> username,
                               "Secret" -> password), null)
    if (response("Response") != "Success") {
      throw new AMIClientException("Login failed: " + response)
    }
  }

  def originate(channel: String,
                context: String,
                extension: String,
                callerid: String,
                timeout_ms: Int,
                priority: Int,
                variables: Map[String, String]) {
    val response =  command(Map("Action" -> "Originate",
                                "Channel" -> channel,
                                "Context" -> context,
                                "Exten" -> extension,
                                "Priority" -> priority.toString,
                                "Callerid" -> callerid,
                                "Timeout" -> timeout_ms.toString), variables)

    if (response("Response") != "Success") {
      throw new AMIClientException("Originate failed: " + response)
    }
  }
}