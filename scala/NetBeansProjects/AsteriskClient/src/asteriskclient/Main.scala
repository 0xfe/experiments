/*
 * Main.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package asteriskclient

import scala.util.logging._;

object Main {
  def main(args: Array[String]) {
    val conn = new AMIConnection("localhost", 7777) with ConsoleLogger
    val gateway = new AMIClient(conn)

    try {
      conn.connect
      val response = gateway.login("mohit", "password")
      println(response.toString)
    } catch {
      case e: AMIConnectionException => println("Not connected.")
    } finally {
      conn.disconnect
    }
  }
}