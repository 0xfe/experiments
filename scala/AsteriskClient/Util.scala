package asterisk.ami

import scala.collection.mutable.Map;
import scala.io.Source;
import scala.util.logging._;
import scala.util.matching.Regex;
import scala.util.Random;

object Util extends Logged {
  private val ACTIONID_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
  private val ResponseEntry = """\s*(\S+)\s*:\s*(.+)\s*""".r
  private val random = new Random()

  val eol = "\r\n"

  // Generates a unique ActionID string for asterisk commands. The string
  // is composed of the characters in ACTIONID_CHARS
  def genActionId: String = (1 to 16).map(
    x => ACTIONID_CHARS(random.nextInt(ACTIONID_CHARS.length))).mkString

  // Generates AMI command variables that can be plugged into the "Variable"
  // key of a command.
  def genVariables(vars: Map[String, String]) : String =
    vars.keys.map(k => k + "=" + vars(k)).mkString("|")

  // Generates an Asterisk AMI command from the given command-map.
  def genCommand(command: Map[String, String],
                 variables: Option[Map[String, String]]) : String = {
    variables.foreach(vars => command += ("Variable" -> genVariables(vars)))
    command.keys.map(k => k + ": " + command(k)).mkString(eol) + eol + eol
  }

  // Parse out an Asterisk response from given string, and return it in a map
  def parseResponse(response: String):Map[String, String]  = {
    val response_map: Map[String, String] = Map()
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
