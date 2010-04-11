/*
 * AsteriskTools.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package asteriskclient

import scala.collection.mutable.Map;
import scala.io.Source;
import scala.util.logging._;
import scala.util.matching.Regex;

object Util extends Logged {
  type AsteriskMessage = Map[String, String]
  private val ACTIONID_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
  private val ResponseEntry = """\s*(\S+)\s*:\s*(\S+)\s*""".r
  private val random = new Random()

  // Generates a unique ActionID string for asterisk commands. The string
  // is composed of the characters in ACTIONID_CHARS
  def genActionId: String = (1 to 16).map(
    x => ACTIONID_CHARS(random.nextInt(ACTIONID_CHARS.length))).mkString

  // Generates an Asterisk AMI command from the given command-map.
  def genCommand(command: AsteriskMessage) : String =
    command.keys.map(k => k + ": " + command(k)).mkString("\n") + "\n\n"

  // Generates AMI command variables that can be plugged into the "Variable"
  // key of a command.
  def genVariables(vars: Map[String, String]) : String =
    vars.keys.map(k => k + "=" + vars(k)).mkString("|")

  // Parse out an Asterisk response from given string, and return it in a map
  def parseResponse(response: String):AsteriskMessage  = {
    val response_map:AsteriskMessage = Map()
    val response_lines = Source.fromString(response).getLines

    for (line <- response_lines) {
      line match {
        case ResponseEntry(key, value) => response_map += (key -> value)
        case _ => // do nothing
      }
    }

    return response_map
  }
}