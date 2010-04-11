package com.muthanna.playground.scala

object ForEach {
  def main(args: Array[String]) {
    args.foreach(arg => println(arg));
    args.foreach(println(_));
  }
}
